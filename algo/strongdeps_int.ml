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

(** Strong Dependencies *)

open ExtLib
open Common

let mainbar = Util.Progress.create "Strongdeps_int.main"
let conjbar = Util.Progress.create "Strongdeps_int.conj"
let strongtimer = Util.Timer.create "Strongdeps_int.strong"
let conjtimer = Util.Timer.create "Strongdeps_int.conjdep"

include Util.Logging(struct let label = __FILE__ end) ;;

module G = Defaultgraphs.PackageGraph.G
module O = Defaultgraphs.PackageGraph.O

(** check if p strongly depends on q.
    We check if it is possible to install p without q.  *)
(* ATT: this function makes a copy of the solver to add a clause to it *)
let strong_depends solver p q =
  Depsolver_int.S.reset solver.Depsolver_int.constraints;
  let solver = Depsolver_int.copy_solver solver in
  let lit = Depsolver_int.S.lit_of_var (solver.Depsolver_int.map#vartoint q) false in
  Depsolver_int.S.add_rule solver.Depsolver_int.constraints [|lit|] [];
  match Depsolver_int.solve solver (Diagnostic_int.Sng (None,p)) with
  |Diagnostic_int.Failure _ -> true
  |Diagnostic_int.Success _ -> false

(** check if [p] strong depends on any packages in [l] *)
let check_strong univ transitive graph solver p l =
  let pkg_p = CudfAdd.inttovar univ p in
  List.iter (fun q ->
    let pkg_q = CudfAdd.inttovar univ q in
    if p <> q then
      if not(G.mem_edge graph pkg_p pkg_q) then
        if strong_depends solver p q then 
          Defaultgraphs.PackageGraph.add_edge ~transitive graph pkg_p pkg_q
  ) l

(* true if at least one dependency is disjunctive *)
let somedisj pool id = 
  let cudfpool = Depsolver_int.strip_cudf_pool pool in
  let (depends,_) = cudfpool.(id) in
  if List.length depends > 0 then
    try
      List.iter (function (_,[_]) -> () | _ -> raise Not_found) depends;
      false
    with Not_found -> true
  else false

(** [strongdeps l] build the strong dependency graph of l *)
(* each package has a node in the graph, even if it does not have  
 * any strong dependencies *)
let strongdeps_int ?(transitive=true) graph univ pkglist =
  let cudfpool = Depsolver_int.init_pool_univ ~global_constraints:false univ in
  Util.Progress.set_total mainbar (List.length pkglist);
  Util.Timer.start strongtimer;
  List.iter (fun pkg ->
    Util.Progress.progress mainbar;
    G.add_vertex graph pkg;
    let id = CudfAdd.vartoint univ pkg in
    if somedisj cudfpool id then begin 
      let closure = Depsolver_int.dependency_closure_cache cudfpool [id] in
      let solver = Depsolver_int.init_solver_closure cudfpool closure in
      match Depsolver_int.solve solver (Diagnostic_int.Sng (None,id)) with
      |Diagnostic_int.Failure(_) -> ()
      |Diagnostic_int.Success(f_int) ->
          check_strong univ transitive graph solver id (f_int ())
    end
  ) pkglist ;
  Util.Progress.reset mainbar;
  debug "strong dep graph: %d nodes, %d edges" (G.nb_vertex graph) (G.nb_edges graph);
  Util.Timer.stop strongtimer graph
;;

let strongdeps ?(transitive=true) univ pkglist =
  let closure = Depsolver.dependency_closure univ pkglist in
  let size = Cudf.universe_size univ in
  let graph = G.create ~size () in
  Util.Progress.set_total conjbar size;

  Util.Timer.start conjtimer;
  List.iter (fun pkg ->
    Util.Progress.progress conjbar;
    Defaultgraphs.PackageGraph.conjdepgraph_int ~transitive graph univ pkg
  ) closure;
  Util.Progress.reset conjbar;
  Util.Timer.stop conjtimer ();

  debug "conj dep graph: nodes %d , edges %d" (G.nb_vertex graph) (G.nb_edges graph);
  let g = strongdeps_int ~transitive graph univ pkglist in
  (* because the graph might still be transitive *)
  (* if not transitive then O.transitive_reduction g; *)
  g
;;

let strongdeps_univ ?(transitive=true) univ =
  let size = Cudf.universe_size univ in
  let graph = G.create ~size () in
  Util.Progress.set_total conjbar size;

  Util.Timer.start conjtimer;
  let l = 
    Cudf.fold_packages (fun acc pkg ->
      Util.Progress.progress conjbar;
      Defaultgraphs.PackageGraph.conjdepgraph_int ~transitive graph univ pkg;
      pkg :: acc
    ) [] univ
  in
  Util.Progress.reset conjbar;
  Util.Timer.stop conjtimer ();
  debug "conj dep graph: nodes %d , edges %d" (G.nb_vertex graph) (G.nb_edges graph);
  let g = strongdeps_int ~transitive graph univ l in
  (* because the graph might still be transitive *)
  (* if not transitive then O.transitive_reduction g; *)
  g
;;

(** return the impact set (list) of the node [q] in [graph] *)
(** invariant : we assume the graph is NOT detransitivitized *)
let impactlist = Defaultgraphs.PackageGraph.pred_list

(** return the list of strong dependencies of the node [q] in [graph] *)
(** invariant : we assume the graph is NOT detransitivitized *)
let stronglist = Defaultgraphs.PackageGraph.succ_list

let impactset = Defaultgraphs.PackageGraph.pred_set

let strongset = Defaultgraphs.PackageGraph.succ_set

