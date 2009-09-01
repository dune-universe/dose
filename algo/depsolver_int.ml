(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

(** Implementation of the EDOS algorithms *)

(* this module respect the cudf semantic. *)

open ExtLib
open Cudf
open Common
open CudfAdd

module R = struct type reason = Diagnostic.reason end
module S = EdosSolver.M(R)

type maps = {
  to_sat : Cudf.package -> S.var ;
  from_sat : S.var -> Cudf.package ;
  who_conflicts : Cudf.package -> Cudf.package list;
  who_provides : Cudf_types.vpkg -> Cudf.package list ;
  lookup_packages : Cudf_types.vpkg -> Cudf.package list
}

(** the sat solver assume a variable ordering *)
let build_maps universe =
  let size = Cudf.universe_size universe in
  let backward_table = Hashtbl.create (2 * size) in
  let forward_table = Cudf_hashtbl.create (2 * size) in
  let conflicts = Cudf_hashtbl.create (2 * size) in
  let provides = Hashtbl.create (2 * size) in 
  let i = ref 0 in

  Cudf.iter_packages (fun pkg ->
    Cudf_hashtbl.add forward_table pkg !i;
    Hashtbl.add backward_table !i pkg;
    incr i;

    List.iter (function
      |name, None -> Hashtbl.add provides name (pkg, None)
      |name, Some (_, ver) -> Hashtbl.add provides name (pkg, (Some ver))
    ) pkg.provides
  ) universe
  ;

  let who_provides (pkgname,constr) =
    List.filter_map (function
      |pkg, None -> Some(pkg)
      |pkg, Some v when Cudf.version_matches v constr -> Some(pkg)
      |_,_ -> None
    ) (Hashtbl.find_all provides pkgname)
  in

  let lookup_packages (pkgname,constr) =
    match Cudf.lookup_packages ~filter:constr universe pkgname with
    |[] -> who_provides (pkgname,constr)
    |l -> l
  in

  Cudf.iter_packages (fun pkg ->
    List.iter (fun (name,constr) ->
      List.iter (fun p ->
        if p <> pkg then begin
          Cudf_hashtbl.add conflicts pkg p ;
          Cudf_hashtbl.add conflicts p pkg
        end
      ) (lookup_packages (name,constr))
    ) pkg.conflicts
  ) universe
  ;

  let who_conflicts pkg = List.unique ~cmp:Cudf.(=%) (Cudf_hashtbl.find_all conflicts pkg) in

  let to_sat =
    try Cudf_hashtbl.find forward_table 
    with Not_found -> assert false
  in

  let from_sat =
    try Hashtbl.find backward_table
    with Not_found -> assert false
  in

  {
    to_sat = to_sat ;
    from_sat = from_sat ;
    who_conflicts = who_conflicts ;
    who_provides = who_provides ;
    lookup_packages = lookup_packages ;
  }
;;

type solver_t = {
  constraints : S.state ;
  size : int ;
  conflicts : int ;
  disjunctions : int ;
  dependencies : int
}

type solver = {
  universe : Cudf.universe ;
  maps : maps ;
  solver : solver_t
}

let init_solver buffer proxy_size (universe,maps) =
  let size = (Cudf.universe_size universe) + proxy_size in

  let progressbar = Util.progress "Depsolver.init_solver" in
  let total = size in
  let i = ref 0 in

  let constraints = S.initialize_problem ~buffer:buffer size in
  let num_conflicts = ref 0 in
  let num_disjunctions = ref 0 in
  let num_dependencies = ref 0 in

  (* add conflicts *)
  let add_conj pkg_id conjunction =
    let x = S.lit_of_var pkg_id false in
    List.iter (fun pkg_id' ->
      if pkg_id <> pkg_id' then begin
        num_conflicts := !num_conflicts + 2;
        let y = S.lit_of_var pkg_id' false in
        S.add_bin_rule constraints x y 
        [Diagnostic.Conflict(maps.from_sat pkg_id, maps.from_sat pkg_id')]
      end
    ) conjunction
  in
  (* add dependencies *)
  let add_cnf pkg_id conjunction =
    List.iter (fun (vpkgs,disjunction) ->
      let lit = S.lit_of_var pkg_id false in
      incr num_dependencies ;
      if List.length disjunction > 0 then begin
        let lit_list =
          List.map (fun v ->
            incr num_disjunctions;
            S.lit_of_var v true
          ) disjunction
        in
        S.add_rule constraints
          (Array.of_list (lit :: lit_list))
          [Diagnostic.Dependency(maps.from_sat pkg_id, List.map maps.from_sat disjunction)]
      end
      else
        S.add_un_rule constraints lit
        [Diagnostic.EmptyDependency(maps.from_sat pkg_id,vpkgs)]
      ;
      if List.length disjunction > 1 then
        S.associate_vars constraints (S.lit_of_var pkg_id true) disjunction
    ) conjunction
  in

  let exec_depends pkg =
    let pid = maps.to_sat pkg in
    let conjunction =
      List.map (fun disjunction ->
        List.fold_left (fun (l1,l2) vpkg ->
          let el = List.map (fun pkg -> maps.to_sat pkg) (maps.lookup_packages vpkg) in
          (vpkg::l1,el @ l2)
        ) ([],[]) disjunction
      ) pkg.depends
    in add_cnf pid conjunction
  in
  let exec_conflicts pkg =
    let pid = maps.to_sat pkg in
    let l = (List.map (fun p -> maps.to_sat p) (maps.who_conflicts pkg)) in
    add_conj pid l
  in

  Cudf.iter_packages (fun pkg ->
    progressbar (incr i ; !i , total) ;
    exec_depends pkg ; 
    exec_conflicts pkg 
  ) universe ;

  S.propagate constraints ;

  {
    constraints = constraints ;
    size = size ;
    conflicts = !num_conflicts ;
    dependencies = !num_dependencies ;
    disjunctions = !num_disjunctions
  }
;;

let init buffer universe =
  match Cudf_checker.is_consistent universe with
  |true,None -> 
      let maps = build_maps universe in
      let solver = init_solver buffer 0 (universe,maps) in
      (solver,maps)
  |false,Some(r) -> begin
      Printf.eprintf "%s" 
      (Cudf_checker.explain_reason (r :> Cudf_checker.bad_solution_reason)) ;
      exit(1)
  end
  |_,_ -> assert false
;;

let solve (solver,maps) request =
  S.reset solver.constraints;

  let result solve collect var =
    if solve solver.constraints var then begin
      let get_assignent () = 
        let l = ref [] in
        Array.iteri(fun i a ->
          if (i <> solver.size - 1) && (a = S.True) then
            let pkg = (maps.from_sat i) in
            let pkg = {pkg with installed = true} in
            l := pkg::!l 
        ) (S.assignment solver.constraints)
        ;
        !l
      in 
      Diagnostic.Success(get_assignent)
    end
    else
      let get_reasons () = collect solver.constraints var in
      Diagnostic.Failure(get_reasons)
  in

  match request with
    |Diagnostic.Req ->
        let res = result S.solve S.collect_reasons (solver.size - 1) in
        { Diagnostic.result = res ; request = request }
    |Diagnostic.Sng r ->
        let res = result S.solve S.collect_reasons (maps.to_sat r) in
        { Diagnostic.result = res ; request = request }
    |Diagnostic.Lst rl ->
        let res =
          result S.solve_lst S.collect_reasons_lst (List.map maps.to_sat rl) in
        { Diagnostic.result = res ; request = request }
;;

(***********************************************************)

let edos_install (solver,maps) request = solve (solver,maps) (Diagnostic.Sng request) ;;
let edos_coinstall (solver,maps) request_lst = solve (solver,maps) (Diagnostic.Lst request_lst) ;;

let pkgcheck callback (solver,maps) failed tested pkg =
  try
  if not(tested.(maps.to_sat pkg)) then begin
    let r = edos_install (solver,maps) pkg in
    begin match r with
    |{ Diagnostic.result = Diagnostic.Success(l) } -> 
        begin try List.iter (fun i -> tested.(maps.to_sat i) <- true) (l ())
        with Not_found -> assert false end
    |_ -> incr failed
    end
    ;
    match callback with
    |None -> ()
    |Some f -> f r
  end
  else if tested.(maps.to_sat pkg) then ()
  else assert false
  with Not_found -> ()

(* callback : Diagnostic.result -> unit *)
let distribcheck ?callback (solver,maps) universe =
  let timer = Util.Timer.create "Algo.Depsolver.distribcheck" in
  Util.Timer.start timer;
  let failed = ref 0 in
  let tested = Array.make solver.size false in
  let check = pkgcheck callback (solver,maps) failed tested in
  Cudf.iter_packages check universe ;
  Util.Timer.stop timer !failed
;;

let pkglistcheck ?callback (solver,maps) pkglist =
  let timer = Util.Timer.create "Algo.Depsolver.pkglistcheck" in
  Util.Timer.start timer;
  let failed = ref 0 in
  let tested = Array.make solver.size false in
  let check = pkgcheck callback (solver,maps) failed tested in
  List.iter check pkglist ;
  Util.Timer.stop timer !failed
;;

(***********************************************************)
(* everything can end in tears if there is a package in l that was not
 * indexed in maps *)
let dependency_closure maps l =
  let module S = Set.Make(struct type t = Cudf.package let compare = compare end) in
  let queue = Queue.create () in
  let visited = ref S.empty in
  List.iter (fun e -> Queue.add e queue) l;
  while (Queue.length queue > 0) do
    let root = Queue.take queue in
    visited := S.add root !visited;
    List.iter (fun disjunction ->
      List.iter (fun (pkgname,constr) ->
        List.iter (fun pkg ->
          if not (S.mem pkg !visited) then
            Queue.add pkg queue
        ) (maps.lookup_packages (pkgname,constr))
      ) disjunction
    ) root.depends
  done;
  S.elements !visited
;;

let conflict_closure maps l =
  let module S = Set.Make(struct type t = Cudf.package let compare = compare end) in
  let conflicts = 
    List.fold_left (fun acc pkg ->
      let l = maps.who_conflicts pkg in
      List.fold_left (fun set p -> S.add p set) acc l
    ) S.empty l
  in
  S.elements conflicts
;;
