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

(** Strong Dependencies *)

open Graph
open ExtLib
open Common
open CudfAdd

(** progress bar *)
let mainbar = Util.Progress.create "Algo.Strongdep.main"

(** progress bar *)
let conjbar = Util.Progress.create "Algo.Strongdep.conj"

(* I'm not using Defaultgraphs.IntGraph because otherwise I would
 * need to provide a printing function making this code less elegant
 * \methink *)
module PkgV = struct
    type t = int
    let compare = Pervasives.compare
    let hash i = i
    let equal = (=)
end
module G = Imperative.Digraph.ConcreteBidirectional(PkgV)
module SO = Defaultgraphs.GraphOper(G)

(** add to the graph all conjunctive dependencies of package id *)
let conjdepgraph_int graph index id =
  G.add_vertex graph id;
  Array.iter (function
    |(_,[|p|],_) -> G.add_edge graph id p
    | _ -> ()
  ) index.(id).Mdf.depends

(** for all if in idlist add to the graph all conjunctive dependencies *)
let conjdepgraph index idlist =
  let graph = G.create ~size:(List.length idlist) () in
  List.iter (conjdepgraph_int graph index) idlist ;
  graph

(** given a graph return the conjunctive dependency closure of the package id *)
let conjdeps graph id =
  let module Dfs = Traverse.Dfs(G) in
  let l = ref [] in
  let collect id = l := id :: !l in
  Dfs.prefix_component collect graph id;
  !l

open Depsolver_int
open G

(** check if p strong dependens on q.
    We check if it is possible to install p without q.  *)
(* ATT: this function makes a copy of the solver to add a clause to it *)
let strong_depends solver p q =
  Depsolver_int.S.reset solver.constraints; 
  let solver = Depsolver_int.copy_solver solver in 
  let lit = Depsolver_int.S.lit_of_var (solver.map#vartoint q) false in
  Depsolver_int.S.add_un_rule solver.constraints lit [];
  match Depsolver_int.solve solver (Diagnostic_int.Sng p) with
  |Diagnostic_int.Failure _ -> true
  |Diagnostic_int.Success _ -> false

(** check if [p] strong depends on any packages in [l] *)
let check_strong graph solver p l =
  List.iter (fun q ->
    if not(p = q) then
      if not(G.mem_edge graph p q) then
        if strong_depends solver p q then 
          G.add_edge graph p q
  ) l

let allconj depends = 
  if Array.length depends > 0 then
    try
      Array.iter (function (_,[|_|],_) -> () | _ -> raise Not_found) depends;
      false
    with Not_found -> true
  else true

(** [strongdeps l] build the strong dependency graph of l *)
(* each package is in the graph, even if it does not have  
 * any strong dependencies *)
let strongdeps_int graph mdf l =
  let available = List.sort ~cmp:(fun (_,n,_) (_,m,_) -> m - n) l in
  let size = List.length available in

  Common.Util.print_info "Conjunctive : nodes %d , edges %d"
  (G.nb_vertex graph) (G.nb_edges graph);
  let g1 = (SO.O.add_transitive_closure graph) in
  Common.Util.print_info "Conjunctive transitive: nodes %d , edges %d"
  (G.nb_vertex g1) (G.nb_edges g1);

  Util.Progress.set_total mainbar size;
  let strongtimer = Util.Timer.create "Algo.Strongdep.strong" in

  Util.Timer.start strongtimer;
  List.iter (fun (pkg1,_,closure) ->
    let pkg1_id = pkg1.Mdf.id in
    G.add_vertex graph pkg1_id;
    Util.Progress.progress mainbar;
    if allconj pkg1.Mdf.depends then begin
      let solver = Depsolver_int.init_solver ~idlist:closure mdf.Mdf.index in
      match Depsolver_int.solve solver (Diagnostic_int.Sng pkg1_id) with
      |Diagnostic_int.Failure(_) -> ()
      |Diagnostic_int.Success(f) -> check_strong graph solver pkg1_id (f ())
    end
  ) available ;
  Util.Timer.stop strongtimer (SO.O.add_transitive_closure graph)

(* XXX this can be refactored in a better way ... *)
let strongdeps mdf idlist =
  let graph = G.create () in
  let size = List.length idlist in
  Util.Progress.set_total conjbar size;
  let conjtimer = Util.Timer.create "Algo.Strongdep.conjdep" in

  Util.Timer.start conjtimer;
  let l = 
    List.fold_left (fun acc id ->
      let pkg = mdf.Mdf.index.(id) in
      Util.Progress.progress conjbar;
      conjdepgraph_int graph mdf.Mdf.index id; 
      let closure = dependency_closure mdf [id] in
      (pkg,List.length closure,closure) :: acc
    ) [] idlist
  in
  Util.Timer.stop conjtimer ();
  strongdeps_int graph mdf l

(* XXX this can be refactored in a better way ... *)
let strongdeps_univ mdf =
  let graph = G.create () in
  let size = Array.length mdf.Mdf.index in
  Util.Progress.set_total conjbar size;
  let conjtimer = Util.Timer.create "Algo.Strongdep.conjdep" in

  Util.Timer.start conjtimer;
  let l = 
    let id = ref 0 in
    Array.fold_left (fun acc pkg ->
      Util.Progress.progress conjbar;
      conjdepgraph_int graph mdf.Mdf.index !id;
      let closure = dependency_closure mdf [!id] in
      incr id ;
      (pkg,List.length closure,closure) :: acc
    ) [] mdf.Mdf.index
  in
  Util.Timer.stop conjtimer ();
  strongdeps_int graph mdf l

(* we assume the graph is NOT detransitivitized *)
let impactset graph q =
  G.fold_pred (fun p acc -> p :: acc ) graph q []
