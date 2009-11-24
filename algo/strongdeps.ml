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

open ExtLib
open Common
open CudfAdd

(** transform an integer graph in a cudf graph *)
let intcudf index intgraph =
  let module PG = Defaultgraphs.PackageGraph.G in
  let module SG = Strongdeps_int.G in
  let trasformtimer = Util.Timer.create "Algo.Strongdep.intcudf" in
  Util.Timer.start trasformtimer;
  let cudfgraph = PG.create () in
  SG.iter_edges (fun x y ->
    let p = index.(x) in
    let q = index.(y) in
    PG.add_edge cudfgraph p.Mdf.pkg q.Mdf.pkg
  ) intgraph ;
  SG.iter_vertex (fun v ->
    let p = index.(v) in
    PG.add_vertex cudfgraph p.Mdf.pkg
  ) intgraph ;
  Common.Util.print_info "cudfgraph: nodes %d , edges %d" 
  (PG.nb_vertex cudfgraph) (PG.nb_edges cudfgraph); 
  Util.Timer.stop trasformtimer cudfgraph

let cudfint maps cudfgraph =
  let module PG = Defaultgraphs.PackageGraph.G in
  let module SG = Strongdeps_int.G in
  let trasformtimer = Util.Timer.create "Algo.Strongdep.cudfint" in
  Util.Timer.start trasformtimer;
  let intgraph = SG.create () in
  PG.iter_edges (fun x y ->
    SG.add_edge intgraph
    (maps.map#vartoint x) (maps.map#vartoint y)
  ) cudfgraph;
  PG.iter_vertex (fun v ->
    SG.add_vertex intgraph (maps.map#vartoint v)
  ) cudfgraph;
  Common.Util.print_info "intcudf: nodes %d , edges %d" 
  (SG.nb_vertex intgraph) (SG.nb_edges intgraph); 
  Util.Timer.stop trasformtimer intgraph

(** [strongdeps u l] build the strong dependency graph of all packages in 
    [l] wrt the universe [u] *)
let strongdeps universe pkglist =
  let mdf = Mdf.load_from_universe universe in
  let maps = mdf.Mdf.maps in
  let idlist = List.map maps.map#vartoint pkglist in
  let g = Strongdeps_int.strongdeps mdf idlist in
  intcudf mdf.Mdf.index g

(** [strongdeps_univ u] build the strong dependency graph of 
all packages in the universe [u]*)
let strongdeps_univ universe =
  let mdf = Mdf.load_from_universe universe in
  let g = Strongdeps_int.strongdeps_univ mdf in
  intcudf mdf.Mdf.index g

(** compute the impact set of the node [q], that is the list of all 
    packages [p] that strong depends on [q] *)
let impactset graph q =
  let module G = Defaultgraphs.StrongDepGraph.G in
  G.fold_pred (fun p acc -> p :: acc ) graph q []

(** compute the conjunctive dependency graph *)
let conjdeps_univ universe =
  let mdf = Mdf.load_from_universe universe in
  let g = Strongdeps_int.G.create () in
  for id=0 to (Array.length mdf.Mdf.index)-1 do
    Strongdeps_int.conjdepgraph_int g mdf.Mdf.index id
  done;
  let clousure = Strongdeps_int.SO.O.add_transitive_closure g in
  intcudf mdf.Mdf.index clousure

(** compute the conjunctive dependency graph considering only packages in
    [pkglist] *)
let conjdeps universe pkglist =
  let mdf = Mdf.load_from_universe universe in
  let maps = mdf.Mdf.maps in
  let idlist = List.map maps.map#vartoint pkglist in
  let g = Strongdeps_int.G.create () in
  List.iter (fun id ->
    Strongdeps_int.conjdepgraph_int g mdf.Mdf.index id
  ) idlist ;
  let clousure = Strongdeps_int.SO.O.add_transitive_closure g in
  intcudf mdf.Mdf.index clousure

