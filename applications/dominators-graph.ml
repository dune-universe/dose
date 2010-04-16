(**************************************************************************************)
(*  Copyright (C) 2010 Jaap Boender <boender@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Common
open Cudf
open Graph
open Defaultgraphs
open Cudf_types_pp

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let tarjan = StdOpt.store_true ()

  let description = "Compute the dominator graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'t' ~long_name:"tarjan" ~help:"Use Tarjan algorithm" tarjan;
end

module G = StrongDepGraph.G;;
module O = StrongDepGraph.O;;
module C = Components.Make(G);;

(* ----------------------------------- *)

module S = Set.Make(struct type t = string * string let compare = compare end)
module SG = PackageGraph.G
module SO = PackageGraph.O;;
module Dom = Dominators.Make(SG)
module D = PackageGraph.D;;

let () =
begin
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ();
  let (universe,_,_) = Boilerplate.load_universe posargs in

  let sd_graph = Strongdeps.strongdeps_univ universe in
  let dom_graph =
    if OptParse.Opt.get Options.tarjan then
      Dom.dominators_tarjan sd_graph
    else
      Dom.dominators sd_graph in
  let old_dom_graph = Defaultgraphs.StrongDepGraph.DIn.parse "dominators-old.dot" in
    SG.iter_edges (fun a b ->
      if not (G.mem_edge old_dom_graph (a.package,"") (b.package,"")) then
        Printf.printf "New edge: %s (pred: %s) -> %s\n" (string_of_pkgname a.package) (String.concat "," (List.map (fun p -> p.package) (SG.pred dom_graph a))) (string_of_pkgname b.package)
    ) dom_graph 
end;;
