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

include Util.Logging(struct let label = __FILE__ end) ;;

(** [strongdeps u l] build the strong dependency graph of all packages in 
    [l] wrt the universe [u] *)
let strongdeps ?(transitive=true) universe pkglist =
  Strongdeps_int.strongdeps ~transitive universe pkglist

(** [strongdeps_univ u] build the strong dependency graph of 
    all packages in the universe [u] *)
let strongdeps_univ ?(transitive=true) universe =
  Strongdeps_int.strongdeps_univ ~transitive universe

(** compute the impact set of the node [q], that is the list of all 
    packages [p] that strong depends on [q] *)
let impactset = Defaultgraphs.PackageGraph.pred_list

(** compute the conjunctive dependency graph *)
let conjdeps_univ universe =
  let g = Defaultgraphs.PackageGraph.G.create () in
  Cudf.iter_packages (fun pkg ->
    Defaultgraphs.PackageGraph.conjdepgraph_int g universe pkg
  ) universe;
  g

(** compute the conjunctive dependency graph considering only packages 
    in [pkglist] *)
let conjdeps universe pkglist =
  let g = Defaultgraphs.PackageGraph.G.create () in
  List.iter (fun pkg ->
    Defaultgraphs.PackageGraph.conjdepgraph_int g universe pkg
  ) pkglist ;
  g
