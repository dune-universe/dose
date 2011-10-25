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
open CudfAdd

let debug fmt = Util.make_debug "StrongDeps" fmt
let info fmt = Util.make_info "StrongDeps" fmt
let warning fmt = Util.make_warning "StrongDeps" fmt

(** [strongdeps u l] build the strong dependency graph of all packages in 
    [l] wrt the universe [u] *)
let strongdeps universe pkglist =
  let idlist = List.map (CudfAdd.vartoint universe) pkglist in
  let g = Strongdeps_int.strongdeps universe idlist in
  Defaultgraphs.intcudf universe g

(** [strongdeps_univ u] build the strong dependency graph of 
    all packages in the universe [u] *)
let strongdeps_univ ?(transitive=true) universe =
  let g = Strongdeps_int.strongdeps_univ ~transitive universe in
  Defaultgraphs.intcudf universe g

(** compute the impact set of the node [q], that is the list of all 
    packages [p] that strong depends on [q] *)
let impactset graph q =
  let module G = Defaultgraphs.PackageGraph.G in
  G.fold_pred (fun p acc -> p :: acc ) graph q []

(** compute the (transitive closure of) the conjunctive dependency graph *)
let conjdeps_univ universe =
  let g = Defaultgraphs.IntPkgGraph.G.create () in
  for id=0 to (Cudf.universe_size universe) - 1 do
    Defaultgraphs.IntPkgGraph.conjdepgraph_int g universe id
  done;
  Defaultgraphs.intcudf universe g

(** compute the transitive closure of the conjunctive dependency graph 
    considering only packages in [pkglist] *)
let conjdeps universe pkglist =
  let idlist = List.map (CudfAdd.vartoint universe) pkglist in
  let g = Defaultgraphs.IntPkgGraph.G.create () in
  List.iter (fun id ->
    Defaultgraphs.IntPkgGraph.conjdepgraph_int g universe id
  ) idlist ;
  Defaultgraphs.intcudf universe g
