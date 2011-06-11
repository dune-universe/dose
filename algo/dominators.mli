(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):                                                       *)
(*    Copyright (C) 2009-2011 Pietro Abate <pietro.abate@pps.jussieu.fr>  *)
(*                                                                        *)
(*  Contributor(s):                                                       *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

module G = Defaultgraphs.PackageGraph.G
module S = CudfAdd.Cudf_set

(* the dominators are computed on the strong dependency graph
 *  * with transitive archs *)
val dominators : ?relative:float -> G.t -> G.t

(** clique reduction: replace any clique by a fresh node.
    If we do this before transitive reduction, any cycle will be a clique
    and thus this will also perform cycle reduction. *)
val clique_reduction : G.t -> unit

(* inspired by has_cycle from ocamlgraph; not in hashtbl: not visited,
 * false in hashtbl: visited in another component, true in hashtbl:
 * visited here *)
val cycle_reduction : G.t -> unit

(** compute package dominators using a tarjan-like algorithm *)
val dominators_tarjan : G.t -> G.t
