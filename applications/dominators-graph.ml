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
    (* this should not be necessary, but we'll look at that later on *)
    SG.iter_vertex (fun v ->
      SG.remove_edge sd_graph v v
    ) sd_graph;
  let dom_graph =
    if OptParse.Opt.get Options.tarjan then
      Dom.dominators_tarjan sd_graph
    else
      Dom.dominators sd_graph in
  D.output_graph stdout dom_graph
end;;
