(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Common

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()

  let description = "Compute the strong dependency graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
end

(* ----------------------------------- *)


let predbar = Util.Progress.create "Strongpred"
let debug fmt = Util.make_debug "Strongpred" fmt
let info fmt = Util.make_info "Strongpred" fmt
let warning fmt = Util.make_warning "Strongpred" fmt

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

(* build a mapping between package names and a maps old version -> new version *)
let build_table universe =
  let version_table = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (init_versions_table version_table) universe;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Hashtbl.iter (fun k {contents=l} ->
    let c = ref 1 in
    let hv = Hashtbl.create (List.length l) in
    List.iter (fun n ->
      (* Printf.eprintf "%s : %d -> %d\n" k n (2 * !c); *)
      Hashtbl.add hv n (2 * !c);
      c := !c + 1
    ) (List.sort (List.unique l));
    Hashtbl.add h k hv
  ) version_table;
  h

(* map the old universe in a new universe where all versions are even *)
let renumber universe = 
  let add table name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  in
  let h = build_table universe in
  let rh = Hashtbl.create (Cudf.universe_size universe) in
  let conj_map hv =
    List.map (fun (name,sel) ->
      match sel with
      |None -> (name,sel)
      |Some(c,v) -> begin
        let hv = Hashtbl.find h name in
        add rh name ((c :> Cudf_types.relop),Hashtbl.find hv v);
        (name,Some(c,Hashtbl.find hv v))
      end
    )
  in
  let cnf_map h =
    List.map (fun disjunction ->
      List.map (fun (name,sel) ->
        match sel with
        |None -> (name,sel)
        |Some(c,v) -> begin
          (* Printf.eprintf "->>>>>>>> %s %d\n" name v; *)
          let hv = Hashtbl.find h name in
          add rh name (c,Hashtbl.find hv v);
          (name,Some(c,Hashtbl.find hv v))
        end 
      ) disjunction
    )
  in
  let pkglist = 
    Cudf.fold_packages (fun acc pkg ->
      let hv = try Hashtbl.find h pkg.Cudf.package with Not_found -> assert false in
      let p = 
        { pkg with
          Cudf.version = (try Hashtbl.find hv pkg.Cudf.version with Not_found -> assert false);
          Cudf.depends = (try cnf_map h pkg.Cudf.depends with Not_found -> assert false);
          Cudf.conflicts = (try conj_map h pkg.Cudf.conflicts with Not_found -> assert false);
          Cudf.provides = (try conj_map h pkg.Cudf.provides with Not_found -> assert false)
        }
      in p::acc
    ) [] universe 
  in (rh,pkglist)

let string_of_relop = function
  |`Eq -> "="
  |`Neq -> "!="
  |`Geq -> ">="
  |`Gt -> ">"
  |`Leq -> "<="
  |`Lt -> "<"

let string_of_package p =
  Printf.sprintf "%s - %d" (CudfAdd.string_of_package p) p.Cudf.version

let prediction universe =
  (* print_endline "------------------------------------------";
  print_endline "Original universe";
  Format.printf "%a@\n" Cudf_printer.pp_universe universe; *)
  let (version_table,pkglist) = renumber universe in
  let size = Cudf.universe_size universe in
  let res = Hashtbl.create size in

  (* print_endline "------------------------------------------";
  print_endline "Universe 1.1 ";
  Format.printf "%a@\n" Cudf_printer.pp_packages pkglist; *)
  (*
  let pp_list fmt l =
    List.iter (fun (c,v) ->
      Format.printf "(%s %d)@," (string_of_relop c) v
    ) (List.unique l)
  in
  Hashtbl.iter (fun k { contents = l} ->
    Format.printf "@[<v 1>%s%a@]@\n" k pp_list l
  ) version_table;
  print_endline "------------------------------------------";
  *)

  let universe = Cudf.load_universe pkglist in
  let graph = Strongdeps.strongdeps_univ universe in
  let changed h p =
    try incr (Hashtbl.find h p) 
    with Not_found -> Hashtbl.add h p (ref 1)
  in
  let mem_package univ (p,v) =
    try ignore(Cudf.lookup_package univ (p,v)); true
    with Not_found -> false
  in
  (* function to create a dummy package with a given version and name *)
  let create_dummy univ p = function
     (* FIXME: create an option to decide whether we want to suppress
        analysis of downgrades of p or not; this is the default behaviour
        right now *)
    | v when p.Cudf.version > v -> None 
     (* The version of p in the repository has already been tested *)
    | v when p.Cudf.version = v -> None 
    | v -> 
	let offset = (if p.Cudf.version > v then "-1" else "+1") in
	let n = 
	  try (Cudf.lookup_package_property p "number")^offset
	  with Not_found -> Printf.sprintf "%d%s" p.Cudf.version offset
	in
	Some {Cudf.default_package with
              Cudf.package = p.Cudf.package;
              version = v;
              pkg_extra = [("number",`String n)] 
            }
  in
  (* discriminants takes a list of version selectors and provide a minimal list 
     of versions v1,...,vn s.t. all possible combinations of the valuse of the
     version selectors are exhibited *)
  let evalsel v = function
      (`Eq,v') -> v=v'
    | (`Geq,v') -> v>=v'
    | (`Leq,v') -> v<=v'
    | (`Gt,v') -> v>v'
    | (`Lt,v') -> v<v'
    | (`Neq,v') -> v<>v'
  in
  let discriminants sels =
    let rawvl = List.unique (List.map snd sels) in
    let minv,maxv= List.fold_left (fun (mi,ma) v -> (min mi v,max ma v)) (max_int,min_int) rawvl in
    let h = Hashtbl.create 17 in
    let h' = Hashtbl.create 17 in
    for w = minv-1 to maxv+1 do
      let row = List.map (evalsel w) sels in
      if not (Hashtbl.mem h row) then 
	(Hashtbl.add h row w; Hashtbl.add h' w row);
    done;
    Hashtbl.fold (fun k v acc -> k::acc) h' [], h'
    (* FIXME: need also to return the row associated to *any* version
       to be able, later, to avoid computing on version which have
       the same row as a version existing in the repository
     *)
  in
  Util.Progress.set_total predbar size;
  Cudf.iter_packages 
    (fun p ->
      match
	try Hashtbl.find version_table p.Cudf.package
	with Not_found -> ref []
      with 
	(* If no version of p is explicitly dependend upon, then *)
     (* changing the version of p does not change its impact set *)
      |{ contents = [] } -> () 
      |{ contents = l } ->
          Printf.printf "Analysing package %s\n" (CudfAdd.string_of_package p);
          let vl,explain = discriminants (List.unique l) in
          let isp = Strongdeps.impactset graph p in
          let (pl,_) = List.partition (fun z -> not(Cudf.(=%) z p)) pkglist in
	  List.iter 
	    (fun v ->
              (* FIXME: prove the following; if (p,v) and (p,w) are in U, and
                 q implies (p,v); then q is not installable when (p,w) replaces (p,v) *)
              if p.Cudf.version <> v then 
	      if mem_package universe (p.Cudf.package,v) then
		Printf.printf "If we replace %s with version %d, then all its impact set becomes uninstallable.\n"
		  (string_of_package p) v
	      else
		match create_dummy universe p v with
		|None -> ()
		|Some dummy ->
		    Util.Progress.progress predbar;
		    let u = Cudf.load_universe (dummy::pl) in
		    let s = Depsolver.load u in
		    begin
		      List.iter
			(fun q ->
			  let d = Depsolver.edos_install s q in
			  if not(Diagnostic.is_solution d) then begin
			    Printf.printf "Package %s is in the IS of %s\n" 
			      (string_of_package q) (string_of_package p);
			    if dummy.Cudf.version < p.Cudf.version then
			      Printf.printf "If we downgrade %s to %s then %s is not installable anymore\n"
				(string_of_package p) (string_of_package dummy)
				(string_of_package q)
			    else if dummy.Cudf.version > p.Cudf.version then
			      Printf.printf "If we upgrade %s to %s then %s is not installable anymore\n"
				(string_of_package p) (string_of_package dummy)
				(string_of_package q)
			    else assert false
				;
                            (* FIXME: use explain to detail the constraints that get violated by moving to version v *)
			    changed res p
			  end
			) isp
		    end
	    ) vl
    ) universe;
  Util.Progress.reset predbar;
  res 
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Strongdeps_int.main";"Strongdeps_int.conj"] in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ~bars () ;
  let (universe,_,_) = Boilerplate.load_universe posargs in
  prediction universe
(*
  let outch = open_out "pred.table" in
  List.iter (fun (p,diff,s,d) ->
    let pkg = CudfAdd.print_package p in
    Printf.fprintf outch "%s , %d, %d, %d\n" pkg s d diff
  ) (List.sort ~cmp:(fun (_,x,_,_) (_,y,_,_) -> y - x) l);
  close_out outch
*)
;;

main();;


(* garbage collector *)

(* a function to dump the results of dsicriminants:

  Hashtbl.iter (fun v row -> Printf.printf "Version %d gives " v; List.iter (fun b -> Printf.printf "%b " b) row;print_newline()) expl;; 

Example:

# let vl,expl = discriminants [(`Eq,2);(`Eq,4)];;
val vl : int list = [4; 2; 1]
val expl : (int, bool list) Hashtbl.t = <abstr>
# Hashtbl.iter (fun v row -> Printf.printf "Version %d gives " v; List.iter (fun b -> Printf.printf "%b " b) row;print_newline()) expl;;
Version 1 gives false false 
Version 2 gives true false 
Version 4 gives false true 


*)
