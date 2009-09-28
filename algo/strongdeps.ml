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
let transform index intgraph =
  let trasformtimer = Util.Timer.create "Algo.Strongdep.transform" in
  Util.Timer.start trasformtimer;
  let cudfgraph = Defaultgraphs.PackageGraph.G.create () in
  Strongdeps_int.G.iter_edges (fun x y ->
    let p = index.(x) in
    let q = index.(y) in
    Defaultgraphs.PackageGraph.G.add_edge cudfgraph p.Mdf.pkg q.Mdf.pkg
  ) intgraph ;
  Util.Timer.stop trasformtimer cudfgraph

(** [strongdeps u l] build the strong dependency graph of the all packages in 
    [l] wrt the universe [u] *)
let strongdeps universe pkglist =
  let mdf = Mdf.load_from_universe universe in
  let maps = mdf.Mdf.maps in
  let idlist = List.map maps.map#vartoint pkglist in
  let g = Strongdeps_int.strongdeps mdf idlist in
  transform mdf.Mdf.index g

let strongdeps_univ universe =
  let mdf = Mdf.load_from_universe universe in
  let g = Strongdeps_int.strongdeps_univ mdf in
  transform mdf.Mdf.index g

