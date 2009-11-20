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
  let trasformtimer = Util.Timer.create "Algo.Strongdep.intcudf" in
  Util.Timer.start trasformtimer;
  let cudfgraph = Defaultgraphs.PackageGraph.G.create () in
  Strongdeps_int.G.iter_vertex (fun v ->
    let p = index.(v) in
    Defaultgraphs.PackageGraph.G.add_vertex cudfgraph p.Mdf.pkg
  ) intgraph ;
  Strongdeps_int.G.iter_edges (fun x y ->
    let p = index.(x) in
    let q = index.(y) in
    Defaultgraphs.PackageGraph.G.add_edge cudfgraph p.Mdf.pkg q.Mdf.pkg
  ) intgraph ;
  Util.Timer.stop trasformtimer cudfgraph

let cudfint maps cudfgraph =
  let trasformtimer = Util.Timer.create "Algo.Strongdep.cudfint" in
  Util.Timer.start trasformtimer;
  let intgraph = Strongdeps_int.G.create () in
  Defaultgraphs.PackageGraph.G.iter_vertex (fun v ->
    Strongdeps_int.G.add_vertex intgraph (maps.map#vartoint v)
  ) cudfgraph;
  Defaultgraphs.PackageGraph.G.iter_edges (fun x y ->
    Strongdeps_int.G.add_edge intgraph
    (maps.map#vartoint x) (maps.map#vartoint y)
  ) cudfgraph;
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

let conjdeps universe =
  let mdf = Mdf.load_from_universe universe in
  let maps = mdf.Mdf.maps in
  let g = Strongdeps_int.G.create () in
  Cudf.iter_packages (fun pkg ->
    let id = maps.map#vartoint pkg in
    Strongdeps_int.conjdepgraph_int g mdf.Mdf.index id
  ) universe
  ;
  intcudf mdf.Mdf.index g

