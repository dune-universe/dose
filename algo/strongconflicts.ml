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

open Common
open CudfAdd
open Defaultgraphs

module ICG = Strongconflicts_int.CG

type cfl_type = Explicit | Conjunctive | Other of Diagnostic.reason list

module CflE = struct
  type t = Cudf.package * Cudf.package * cfl_type
  let compare = Pervasives.compare
  let default = (Cudf.default_package, Cudf.default_package, Other [])
end

module CG = Graph.Imperative.Graph.ConcreteLabeled(PackageGraph.PkgV)(CflE)

(* tempy. *)
let reason maps =
  let from_sat = maps.map#inttovar in
  List.map (function
    |Diagnostic_int.Dependency(i,vl,il) ->
      Diagnostic.Dependency(from_sat i,vl,List.map from_sat il)
    |Diagnostic_int.EmptyDependency(i,vl) ->
      Diagnostic.EmptyDependency(from_sat i,vl)
    |Diagnostic_int.Conflict(i,j) ->
      Diagnostic.Conflict(from_sat i,from_sat j)
  );;

let cvt maps =
  function
  | Strongconflicts_int.Explicit -> Explicit
  | Strongconflicts_int.Conjunctive -> Conjunctive
  | Strongconflicts_int.Other l -> Other (reason maps l);;

(** strongconflicts return the list of all strong conflicts in universe.
    
    invariant: the universe must contain only edos-installable packages : see
    Depsolver.trim.
*)
let strongconflicts universe =
  let g = CG.create () in
  let mdf = Mdf.load_from_universe (Depsolver.trim universe) in
  let maps = mdf.Mdf.maps in
  let ig = Strongconflicts_int.strongconflicts mdf (* idlist *) in
  (* convert output graph *)
  ICG.iter_vertex (fun v -> CG.add_vertex g (maps.map#inttovar v)) ig;
  ICG.iter_edges_e (fun (x, (x', y', l), y) ->
    CG.add_edge_e g (maps.map#inttovar x,
      (maps.map#inttovar x', maps.map#inttovar y', cvt maps l),
      maps.map#inttovar y)
  ) ig;
  g

