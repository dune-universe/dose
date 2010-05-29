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
open CudfAdd

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let tarjan = StdOpt.store_true ()
  let out_file = StdOpt.str_option ()
  let do_clean = StdOpt.store_true ()
  let do_cr = StdOpt.store_true ()
  let c_only = StdOpt.str_option ()
  let c_but = StdOpt.str_option ()

  let description = "Compute the dominator graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'t' ~long_name:"tarjan" ~help:"Use Tarjan algorithm" tarjan;
  add options ~short_name:'o' ~long_name:"output" ~help:"Send output to file" out_file;
  add options ~long_name:"clean" ~help:"Clean up the dominator graph" do_clean;
  add options ~long_name:"clique-reduction" ~help:"(If not-Tarjan) Do clique reduction" do_cr;
  add options ~long_name:"only" ~help:"Output only the cluster(s) containing these packages (comma-separated)" c_only;
  add options ~long_name:"all-but" ~help:"Do not output the cluster(s) containing these packages (comma-separated)" c_but;
end

module G = StrongDepGraph.G;;
module O = StrongDepGraph.O;;
module C = Components.Make(G);;

(* ----------------------------------- *)

module S = Set.Make(struct type t = string * string let compare = Pervasives.compare end)
module SG = PackageGraph.G
module SO = PackageGraph.O;;
module Dom = Dominators.Make(SG)
module D = PackageGraph.D;;

let is graph pkg =
  SG.fold_pred (fun p s -> Cudf_set.add p s) graph pkg (Cudf_set.singleton pkg)
;;

let scons graph pkg =
  SG.fold_succ (fun p s -> Cudf_set.add p s) graph pkg (Cudf_set.singleton pkg)
;;

let clean_graph g = 
begin
  SG.iter_vertex (fun v ->
    if SG.in_degree g v = 0 && SG.out_degree g v = 0 then
      SG.remove_vertex g v
    else if SG.in_degree g v = 0 && SG.out_degree g v = 1 then
      SG.iter_succ (fun v' ->
        if SG.out_degree g v' = 0 then
        begin
          SG.remove_vertex g v;
          SG.remove_vertex g v';
        end
      ) g v
    else if SG.in_degree g v = 1 && SG.out_degree g v = 1 then
      SG.iter_succ (fun v' ->
        if SG.in_degree g v' = 1 && SG.in_degree g v' = 1 then
        SG.iter_succ (fun v'' ->
          if v'' = v then
          begin
            SG.remove_vertex g v;
            SG.remove_vertex g v'
          end
        ) g v'
      ) g v
  ) g
end;;

let () =
begin
  Common.Util.set_verbosity Common.Util.Summary;
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ();
  let (universe,_,_) = Boilerplate.load_universe posargs in

  Common.Util.Progress.enable "Algo.Strongdep.main";

  let dom_graph =
    if OptParse.Opt.get Options.tarjan then
      Dom.dominators_tarjan (Strongdeps.strongdeps_univ ~transitive:false universe)
    else
    begin
      let g = Strongdeps.strongdeps_univ universe in
      if OptParse.Opt.is_set Options.do_cr then Dom.clique_reduction g;
      Dom.dominators g
    end in
  SO.transitive_reduction dom_graph;
  match OptParse.Opt.opt Options.c_only with
  | None -> ();
  | Some _ -> ();
  if OptParse.Opt.get Options.do_clean then clean_graph dom_graph;
  begin
    if OptParse.Opt.is_set Options.out_file then
      D.output_graph (open_out (OptParse.Opt.get Options.out_file)) dom_graph
    else
      D.output_graph stdout dom_graph
  end
end;;
