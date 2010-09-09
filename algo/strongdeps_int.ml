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

let debug fmt = Util.make_debug "Strongdeps_int" fmt
let info fmt = Util.make_info "Strongdeps_int" fmt
let warning fmt = Util.make_warning "Strongdeps_int" fmt

module G = IntPkgGraph.G
module SO = IntPkgGraph.SO

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
let check_strong tr graph solver p l =
  List.iter (fun q ->
    if p <> q then
      if not(G.mem_edge graph p q) then
        if strong_depends solver p q then 
          IntPkgGraph.do_add_edge tr graph p q
  ) l

(* true if at least one dependency is disjunctive *)
let somedisj depends = 
  if List.length depends > 0 then
    try
      List.iter (function (_,[_],_) -> () | _ -> raise Not_found) depends;
      false
    with Not_found -> true
  else false

(** [strongdeps l] build the strong dependency graph of l *)
(* each package has a node in the graph, even if it does not have  
 * any strong dependencies *)
let strongdeps_int ?(transitive=true) graph mdf l =
  let available = List.sort ~cmp:(fun (_,n,_) (_,m,_) -> m - n) l in
  let size = List.length available in

  Util.Progress.set_total mainbar size;
  let strongtimer = Util.Timer.create "Strongdeps_int.strong" in

  Util.Timer.start strongtimer;
  List.iter (fun (pkg,_,closure) ->
    let id = pkg.Mdf.id in
    G.add_vertex graph id;
    Util.Progress.progress mainbar;
    if somedisj pkg.Mdf.depends then begin
      let solver = Depsolver_int.init_solver ~closure mdf.Mdf.index in
      match Depsolver_int.solve solver (Diagnostic_int.Sng id) with
      |Diagnostic_int.Failure(_) -> ()
      |Diagnostic_int.Success(f) -> 
        if transitive then
          check_strong true graph solver id (f ())
        else
          let deps =
            List.filter (fun x ->
              List.exists (fun (_,alt,_) ->
                List.exists (fun y -> y = x) alt
              ) pkg.Mdf.depends
            ) (f ()) 
          in
          check_strong false graph solver id deps
    end
  ) available ;
  Util.Progress.reset mainbar;
  Util.Timer.stop strongtimer graph
;;

(* XXX this can be refactored in a better way ... *)
let strongdeps mdf idlist =
  let graph = G.create () in
  let size = List.length idlist in
  Util.Progress.set_total conjbar size;
  let conjtimer = Util.Timer.create "Strongdeps_int.conjdep" in

  Util.Timer.start conjtimer;
  let l = 
    List.fold_left (fun acc id ->
      let pkg = mdf.Mdf.index.(id) in
      Util.Progress.progress conjbar;
      IntPkgGraph.conjdepgraph_int graph mdf.Mdf.index id; 
      let closure = Depsolver_int.dependency_closure mdf [id] in
      (pkg,List.length closure,closure) :: acc
    ) [] idlist
  in
  Util.Progress.reset conjbar;
  Util.Timer.stop conjtimer ();
  strongdeps_int graph mdf l

(* XXX this can be refactored in a better way ... *)
let strongdeps_univ ?(transitive=true) mdf =
  let graph = G.create () in
  let size = Array.length mdf.Mdf.index in
  Util.Progress.set_total conjbar size;
  let conjtimer = Util.Timer.create "Strongdeps_int.conjdep" in

  Util.Timer.start conjtimer;
  let l = 
    let id = ref 0 in
    Array.fold_left (fun acc pkg ->
      Util.Progress.progress conjbar;
      IntPkgGraph.conjdepgraph_int ~transitive graph mdf.Mdf.index !id;
      let closure = Depsolver_int.dependency_closure mdf [!id] in
      incr id ;
      (pkg,List.length closure,closure) :: acc
    ) [] mdf.Mdf.index
  in
  Util.Progress.reset conjbar;
  Util.Timer.stop conjtimer ();
  strongdeps_int ~transitive graph mdf l

(** return the impact set (list) of the node [q] in [graph] *)
(** invariant : we assume the graph is NOT detransitivitized *)
let impactlist graph q =
  G.fold_pred (fun p acc -> p :: acc ) graph q []

(** return the list of strong dependencies of the node [q] in [graph] *)
(** invariant : we assume the graph is NOT detransitivitized *)
let stronglist graph q =
  G.fold_succ (fun p acc -> p :: acc ) graph q []

module S = Set.Make (struct type t = int let compare = Pervasives.compare end)

let impactset graph q =
  if G.mem_vertex graph q then
    G.fold_pred (fun p acc -> S.add p acc) graph q S.empty
  else S.empty

let strongset graph q =
  if G.mem_vertex graph q then
    G.fold_succ (fun p acc -> S.add p acc) graph q S.empty
  else S.empty

