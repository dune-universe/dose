(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):                                                       *)
(*    Copyright (C) 2009,2010 Pietro Abate <pietro.abate@pps.jussieu.fr>  *)
(*                                                                        *)
(*  Contributor(s):                                                       *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open Common
open ExtLib

let debug fmt = Util.make_debug "Algo.Predictions" fmt
let info fmt = Util.make_info "Algo.Predictions" fmt
let warning fmt = Util.make_warning "Algo.Predictions" fmt

type constr = Cudf_types.relop * Cudf_types.version
type from_t = Cudf_types.pkgname * Cudf_types.version -> Cudf_types.pkgname * string
type to_t = Cudf_types.pkgname * string -> Cudf_types.pkgname * Cudf_types.version

type conversion = {
  universe : Cudf.universe ;
  from_cudf : from_t ;
  to_cudf : to_t ;
  constraints : (string, constr list) Hashtbl.t ;
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

(* collect dependency information *)
let conj_iter f t l =
  List.iter (fun (name,sel) ->
    match sel with
    |None -> ()
    |Some(c,v) -> add_unique t name (f (c,v))
  ) l
let cnf_iter f t ll = List.iter (conj_iter f t) ll 

(** [constraints universe] returns a map between package names
    and an ordered list of constraints where the package name is
    mentioned *) 
let constraints universe =
  let id x = x in
  let constraints_table = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (fun pkg ->
    conj_iter id constraints_table pkg.Cudf.conflicts ;
    conj_iter id constraints_table (pkg.Cudf.provides :> Cudf_types.vpkglist) ;
    cnf_iter id constraints_table pkg.Cudf.depends
  ) universe
  ;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  let elements hv =
    List.sort ~cmp:(fun (_,v1) (_,v2) -> v1 - v2) (
      Hashtbl.fold (fun k v acc -> k::acc) hv []
    )
  in
  Hashtbl.iter (fun n hv -> Hashtbl.add h n (elements hv)) constraints_table;
  h
;;

let all_constraints table pkgname =
  try Hashtbl.find table.constraints pkgname
  with Not_found -> []
;;

(* build a mapping between package names and a map old version -> new version *)
(* using new = old * 2                                                        *)
let build_table universe =
  let version_table = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (fun pkg ->
    add_unique version_table pkg.Cudf.package pkg.Cudf.version;
    cnf_iter snd version_table pkg.Cudf.depends ;
    conj_iter snd version_table pkg.Cudf.conflicts ;
    conj_iter snd version_table pkg.Cudf.provides
  ) universe
  ;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Hashtbl.iter (fun k hv ->
    let new_hv = Hashtbl.create (Hashtbl.length hv) in
    Hashtbl.iter (fun n _ ->
      Hashtbl.add new_hv n (2 * n);
    ) hv;
    Hashtbl.add h k new_hv
  ) version_table;
  h

(* map the old universe in a new universe where all versions are even *)
(* the from_cudf function returns real versions for even cudf ones    *)
(* and a representation of the interval n-1, n+1 for odd cudf ones    *)
let renumber (universe,from_cudf,to_cudf) =
  (* XXX the only difference is that contraints does not contain the versions
   * of real packages ... *)
  let h = build_table universe in
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
  let cnf_map ll = List.map conj_map ll in
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
  let new_universe = Cudf.load_universe pkglist in
  let constr = constraints new_universe in
  let new_from_cudf (p,i) =
    let constr = try List.map snd (Hashtbl.find constr p) with Not_found -> [] in
    let realver = List.map (fun pkg -> pkg.Cudf.version) (Cudf.lookup_packages new_universe p) in
    (* actually I don't care if it is unique ... *)
    let vl = (* Util.list_unique *) (constr@realver) in
    debug "All versions for package %s : %s" p (String.concat "," (List.map string_of_int vl));
    try
      (* Note:  we do this way to run the algo in liner time and not with a sort in log(n) *)
      let (minx,maxx,(before,after)) =
        List.fold_left (fun (x,y,(b,a)) v ->
          if v = i then raise Not_found ;
          let b' = if v > b && v < i then v else b in
          let a' = if v < a && v > i then v else a in
          ((min x v),(max y v),(b',a'))
        ) (max_int,1,(1,max_int)) vl
      in
      (* debug "i = %d, min %d, max = %d , before = %d , after = %d" i minx maxx before after ; *)
      match (before,after) with
      |(w,v) when w < minx -> begin
        (* debug "< %s <<%d>>" (snd (from_cudf (p,(minx/2)))) (minx); *)
        (p, Printf.sprintf "< %s" (snd (from_cudf (p,(minx/2)))))
      end
      |(w,v) when v > maxx -> begin
        (* debug "> %s <<%d>>" (snd (from_cudf (p,(maxx/2)))) (maxx); *)
        (p, Printf.sprintf "> %s" (snd (from_cudf (p,(maxx/2)))))
      end
      |(w,v) -> begin
        let bv = (snd (from_cudf (p,(w/2)))) in
        let av = (snd (from_cudf (p,(v/2)))) in
        (* debug "%s <<%d>> < . < %s <<%d>>" bv (w) av (v); *)
        (p,Printf.sprintf "%s < . < %s" bv av)
      end
    with Not_found -> begin
      assert ((i mod 2) = 0);
      from_cudf (p,(i/2))
    end
  in
  let new_to_cudf (p,v) = (p,2 * (snd(to_cudf (p,v)))) in 
  {
    universe = new_universe;
    from_cudf = Util.memo new_from_cudf;
    to_cudf = new_to_cudf;
    constraints = constr
  }

(* function to create a dummy package with a given version and name  *)
(* and an extra property 'number' with a representation of version v *)
let create_dummy table (name,version) =
  {Cudf.default_package with
   Cudf.package = name;
   version = version;
   pkg_extra = [("number",`String (snd (table.from_cudf (name,version))))]
  }
;;

let seq init f cond =
  let acc = ref init in
  let aux () =
    if cond !acc then begin
      let result = !acc in
      acc := f !acc;
      result
    end
    else raise Enum.No_more_elements
  in Enum.from aux

let aux_downto maxx minx =
  let cond m = m >= minx in
  let f i = i - 1 in
  seq maxx f cond

let range constraints =
  let rawvl = List.unique ~cmp:(fun (_,(x:int)) (_,(y:int)) -> x = y) constraints in
  let (minv, maxv) =
    List.fold_left (fun (mi,ma) (_,v) ->
      (min mi v,max ma v)
    ) (max_int,1) rawvl
  in
  aux_downto (maxv+1) (if minv = 1 then 1 else minv-1)
;;

(* discriminants takes a list of version selectors and provides a minimal list 
   of versions v1,...,vn s.t. all possible combinations of the values of the
   version selectors are exhibited. Each evaluation has only one representative. 
   Optional arguments are provided to allow to limit the analysis to a specific
   set of versions [vl] , or to add extra versions to analyse [vcl]*)
let discriminants ?vl ?vcl constraints =
  let evalsel v = function
    |(`Eq,v') -> v = v'
    |(`Geq,v') -> v >= v'
    |(`Leq,v') -> v <= v'
    |(`Gt,v') -> v > v'
    |(`Lt,v') -> v < v'
    |(`Neq,v') -> v <> v'
  in
  let eval_constr = Hashtbl.create 17 in
  let constr_eval = Hashtbl.create 17 in
  let vl =
    match (vl, vcl) with
    | (None,None) -> range constraints
    | (None,Some el) -> range ((List.map (fun v -> (`Eq,v)) el) @ constraints)
    | (Some vl,_) -> List.enum (List.sort ~cmp:compare vl)
  in
(* XXX refactor this. I've the feeling it's very slow to use replace *)
  Enum.iter (fun v ->
    let eval = List.map (evalsel v) constraints in
    try
      let v_rep = Hashtbl.find eval_constr eval in
      let l = Hashtbl.find constr_eval v_rep in
      Hashtbl.replace constr_eval v_rep (v::l)
    with Not_found -> begin
      Hashtbl.add eval_constr eval v;
      Hashtbl.add constr_eval v []
    end
  ) vl ;
  constr_eval
;;

(** [migrate table v l] migrates all packages in [l] to version [v] *)
let migrate table v l =
  List.map (fun p ->
    if Cudf.mem_package table.universe (p.Cudf.package,v) then p
    else create_dummy table (p.Cudf.package,v)
  ) l
;;
