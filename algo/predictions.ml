
open Common
open ExtLib

let debug fmt = Util.make_debug "Algo.Predictions" fmt
let info fmt = Util.make_info "Algo.Predictions" fmt
let warning fmt = Util.make_warning "Algo.Predictions" fmt

type convertion = {
  universe : Cudf.universe ;
  from_cudf : string * int -> string ;
  to_cudf : string * string -> int ;
  constraints : (string, (Cudf_types.relop * int) list) Hashtbl.t ;
}

let add_unique h k v =
  try
    let vh = Hashtbl.find h k in
    if not (Hashtbl.mem vh v) then
      Hashtbl.add vh v ()
  with Not_found -> begin
    let vh = Hashtbl.create 17 in
    Hashtbl.add vh v ();
    Hashtbl.add h k vh
  end

(** [constraints universe] returns a map between package names
    and an ordered list of constraints where the package name is
    mentioned *) 
let constraints universe =
  let conj_iter table =
    List.iter (fun (name,sel) ->
      match sel with
      |None -> ()
      |Some(c,v) -> add_unique table name (c,v)
    )
  in
  let cnf_iter table = List.iter (conj_iter table) in
  let table = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (fun pkg ->
    (* add table pkg.Cudf.package (`Eq,pkg.Cudf.version); *)
    conj_iter table pkg.Cudf.conflicts ;
    conj_iter table (pkg.Cudf.provides :> Cudf_types.vpkglist) ;
    cnf_iter table pkg.Cudf.depends
  ) universe
  ;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  let elements hv =
    List.sort ~cmp:(fun (_,v1) (_,v2) -> v1 - v2) (
      Hashtbl.fold (fun k v acc -> k::acc) hv []
    )
  in
  Hashtbl.iter (fun n hv -> Hashtbl.add h n (elements hv)) table;
  h
;;

(* collect all possible versions *)
let init_versions_table table =
  let add name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  in
  let conj_iter =
    List.iter (fun (name,sel) ->
      match sel with
      |None -> ()
      |Some(_,version) -> add name version
    )
  in
  let cnf_iter =
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        match sel with
        |None -> ()
        |Some(_,version) -> add name version
      ) disjunction
    )
  in
  fun pkg ->
    add pkg.Cudf.package pkg.Cudf.version;
    conj_iter pkg.Cudf.conflicts ;
    conj_iter (pkg.Cudf.provides :> Cudf_types.vpkglist) ;
    cnf_iter pkg.Cudf.depends
;;

(* build a mapping between package names and a map old version -> new version *)
(* using new = old * 2                                                        *)
let build_table constraints =
  let h = Hashtbl.create (Hashtbl.length constraints) in
  Hashtbl.iter (fun k l ->
    let hv = Hashtbl.create (List.length l) in
    List.iter (fun (_,n) ->
      Hashtbl.add hv n (2 * n);
    ) l;
    Hashtbl.add h k hv
  ) constraints;
  h

(* map the old universe in a new universe where all versions are even *)
(* the from_cudf function returns real versions for even cudf ones    *)
(* and a representation of the interval n-1, n+1 for odd cudf ones    *)
let renumber (universe,from_cudf,to_cudf) =
  let c = constraints universe in
  let h = build_table c in
  let map n v =
    try Hashtbl.find (Hashtbl.find h n) v 
    with Not_found -> assert false
  in
  let conj_map l =
    List.map (fun (name,sel) ->
      match sel with
      |None -> (name,sel)
      |Some(c,v) -> (name,Some(c,map name v))
    ) l
  in
  let cnf_map = List.map conj_map in
  let pkglist =
    Cudf.fold_packages (fun acc pkg ->
      let p =
        { pkg with
          Cudf.version = map pkg.Cudf.package pkg.Cudf.version; 
          Cudf.depends = cnf_map pkg.Cudf.depends;
          Cudf.conflicts = conj_map pkg.Cudf.conflicts;
          Cudf.provides = conj_map pkg.Cudf.provides;
        }
      in p::acc
    ) [] universe
  in
  let new_from_cudf (p,i) =
    if i mod 2 = 0 && i > 2 then
      try from_cudf (p,(i/2)) 
      with Not_found -> 
        Printf.sprintf "<missing version %d for %s!>" i p
    else
      let hi = (try (from_cudf (p,(i/2)))^" <" with Not_found -> "") in
      let low = (try "< "^(from_cudf (p,(i/2+1))) with Not_found -> "") in
      Printf.sprintf "%s . %s" hi low
  in
  let new_to_cudf (p,v) = 2 * (to_cudf (p,v)) in 
  let universe = Cudf.load_universe pkglist in
  {
    universe = universe;
    from_cudf = new_from_cudf;
    to_cudf = new_to_cudf;
    constraints = c
  }

(* function to create a dummy package with a given version and name  *)
(* and an extra property 'number' with a representation of version v *)
let create_dummy table (name,version) =
  let interval table (name,version) =
    let map = table.from_cudf in
    let vl =
      try List.map snd (Hashtbl.find table.constraints name)
      with Not_found -> assert false
    in
    if not (List.mem version vl) then
      try
        let (i,v1) = List.findi (fun _ w -> w > version) vl in
        if i = 0 then 
          Printf.sprintf "(< %s)" (map (name,v1))
        else
          Printf.sprintf "(%s < %s < %s)" 
          (map (name,List.nth vl (i-1)))
          (if (version mod 2) = 0 then map (name,version) else ".")
          (map (name,v1))
      with Not_found ->
        Printf.sprintf "(> %s)" (map (name,(List.last vl)))
    else map (name,version)
  in
  {Cudf.default_package with
   Cudf.package = name;
   version = version;
   pkg_extra = [("number",`String (interval table (name,version)))]
  }
;;

(* discriminants takes a list of version selectors and provide a minimal list 
   of versions v1,...,vn s.t. all possible combinations of the valuse of the
   version selectors are exhibited *)
let discriminants ?(vl=[]) constraints =
  let evalsel v = function
    |(`Eq,v') -> v=v'
    |(`Geq,v') -> v>=v'
    |(`Leq,v') -> v<=v'
    |(`Gt,v') -> v>v'
    |(`Lt,v') -> v<v'
    |(`Neq,v') -> v<>v'
  in
  let eval_constr = Hashtbl.create 17 in
  let constr_eval = Hashtbl.create 17 in
  let add c =
    let e = List.map (evalsel c) constraints in
    if not (Hashtbl.mem eval_constr e) then begin
      Hashtbl.add eval_constr e c;
      Hashtbl.add constr_eval c e
    end
  in
  if vl = [] then begin
    let rawvl = List.unique ~cmp:(fun (_,(x:int)) (_,(y:int)) -> x = y) constraints in
    let (minv, maxv) =
      List.fold_left (fun (mi,ma) (_,v) ->
        (min mi v,max ma v)
      ) (max_int,min_int) rawvl
    in
    for c = maxv+1 downto 0 do add c
    done
  end else
    List.iter add (List.sort ~cmp:(fun v1 v2 -> v2 - v1) vl)
  ;
  Hashtbl.fold (fun k v acc -> k::acc) constr_eval [], constr_eval
;;

(** [migrate table v l] migrates all packages in [l] to version [v] *)
let migrate table v l =
  List.map (fun p ->
    if CudfAdd.mem_package table.universe (p.Cudf.package,v) then p
    else create_dummy table (p.Cudf.package,v)
  ) l
;;
