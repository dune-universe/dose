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

open Graph
open ExtLib
open Common
open CudfAdd
open Defaultgraphs

let mainbar = Util.Progress.create "Strongdeps_int.main"
let conjbar = Util.Progress.create "Strongdeps_int.conj"
let strongtimer = Util.Timer.create "Strongdeps_int.strong"
let conjtimer = Util.Timer.create "Strongdeps_int.conjdep"

let debug fmt = Util.make_debug __FILE__ fmt
let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt

module G = IntPkgGraph.G
module O = IntPkgGraph.O

(** check if p strongly depends on q.
    We check if it is possible to install p without q.  *)
(* ATT: this function makes a copy of the solver to add a clause to it *)
let strong_depends solver p q =
  Depsolver_int.S.reset solver.Depsolver_int.constraints;
  let solver = Depsolver_int.copy_solver solver in
  let lit = Depsolver_int.S.lit_of_var (solver.Depsolver_int.map#vartoint q) false in
  Depsolver_int.S.add_rule solver.Depsolver_int.constraints [|lit|] [];
  match Depsolver_int.solve solver (Diagnostic_int.Sng p) with
  |Diagnostic_int.Failure _ -> true
  |Diagnostic_int.Success _ -> false

(** check if [p] strong depends on any packages in [l] *)
let check_strong transitive graph solver p l =
  List.iter (fun q ->
    if p <> q then
      if not(G.mem_edge graph p q) then
        if strong_depends solver p q then 
          IntPkgGraph.add_edge transitive graph p q
  ) l

(* true if at least one dependency is disjunctive *)
let somedisj pool id = 
  let cudfpool = Depsolver_int.strip_cudf_pool pool in
  let depends = fst(cudfpool.(id)) in
  if List.length depends > 0 then
    try
      List.iter (function (_,[_]) -> () | _ -> raise Not_found) depends;
      false
    with Not_found -> true
  else false

(** [strongdeps l] build the strong dependency graph of l *)
(* each package has a node in the graph, even if it does not have  
 * any strong dependencies *)
let strongdeps_int ?(transitive=true) graph univ l =
  let cudfpool = Depsolver_int.init_pool_univ univ in
  Util.Progress.set_total mainbar (List.length l);
  Util.Timer.start strongtimer;
  List.iter (fun id ->
    Util.Progress.progress mainbar;
    G.add_vertex graph id;
    if somedisj cudfpool id then begin 
      let closure = Depsolver_int.dependency_closure_cache cudfpool [id] in
      let solver = Depsolver_int.init_solver_closure cudfpool closure in
      match Depsolver_int.solve solver (Diagnostic_int.Sng id) with
      |Diagnostic_int.Failure(_) -> ()
      |Diagnostic_int.Success(f) ->
          check_strong transitive graph solver id (f ())
    end
  ) l ;
  Util.Progress.reset mainbar;
  ignore (Util.Timer.stop strongtimer ());
  debug "strong dep graph: %d nodes, %d edges\n" (G.nb_vertex graph) (G.nb_edges graph);
  graph
;;

module S = Set.Make (struct type t = int let compare = Pervasives.compare end)

let strongdeps ?(transitive=true) univ closure =
  let size = Cudf.universe_size univ in
  let graph = G.create ~size () in
  Util.Progress.set_total conjbar size;

  Util.Timer.start conjtimer;
  let l = 
    List.fold_left (fun acc id ->
      Util.Progress.progress conjbar;
      IntPkgGraph.conjdepgraph_int ~transitive graph univ id;
      id :: acc
    ) [] closure
  in
  Util.Progress.reset conjbar;
  Util.Timer.stop conjtimer ();
  debug "conj dep graph: nodes %d , edges %d" (G.nb_vertex graph) (G.nb_edges graph);
  let g = strongdeps_int ~transitive graph univ l in
  if not transitive then O.transitive_reduction g;
  g

let strongdeps_univ ?(transitive=true) univ =
  let size = Cudf.universe_size univ in
  let graph = G.create ~size () in
  Util.Progress.set_total conjbar size;

  Util.Timer.start conjtimer;
  let l = 
    Cudf.fold_packages (fun acc pkg ->
      let id = Cudf.uid_by_package univ pkg in
      Util.Progress.progress conjbar;
      IntPkgGraph.conjdepgraph_int ~transitive graph univ id;
      id :: acc
    ) [] univ
  in
  Util.Progress.reset conjbar;
  Util.Timer.stop conjtimer ();
  debug "conj dep graph: nodes %d , edges %d" (G.nb_vertex graph) (G.nb_edges graph);
  let g = strongdeps_int ~transitive graph univ l in
  if not transitive then O.transitive_reduction g;
  g

(** return the impact set (list) of the node [q] in [graph] *)
(** invariant : we assume the graph is NOT detransitivitized *)
let impactlist graph q =
  G.fold_pred (fun p acc -> p :: acc ) graph q []

(** return the list of strong dependencies of the node [q] in [graph] *)
(** invariant : we assume the graph is NOT detransitivitized *)
let stronglist graph q =
  G.fold_succ (fun p acc -> p :: acc ) graph q []

let impactset graph q =
  if G.mem_vertex graph q then
    G.fold_pred (fun p acc -> S.add p acc) graph q S.empty
  else S.empty

let strongset graph q =
  if G.mem_vertex graph q then
    G.fold_succ (fun p acc -> S.add p acc) graph q S.empty
  else S.empty

