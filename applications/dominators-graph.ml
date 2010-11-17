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
open Algo

module Options = struct
  open OptParse
  let description = "Compute the dominator graph"
  let options = OptParser.make ~description

  include Boilerplate.MakeOptions(struct let options = options end)
  let tarjan = StdOpt.store_true ()
  let out_file = StdOpt.str_option ()
  let do_clean = StdOpt.store_true ()
  let do_cr = StdOpt.store_true ()
  let c_only = StdOpt.str_option ()
  let c_but = StdOpt.str_option ()
  let relative = StdOpt.float_option ()
  let trans_red = StdOpt.store_true ()


  open OptParser
  add options ~short_name:'t' ~long_name:"tarjan" ~help:"Use Tarjan algorithm" tarjan;
  add options ~short_name:'o' ~long_name:"output" ~help:"Send output to file" out_file;
  add options ~long_name:"clean" ~help:"Clean up the dominator graph" do_clean;
  add options ~long_name:"clique-reduction" ~help:"(If not-Tarjan) Do clique reduction" do_cr;
  add options ~long_name:"detransitivise" ~help:"Do transitive reduction" trans_red;
  add options ~short_name:'r' ~long_name:"relative" ~help:"Use relative strong dominance (with percentage)" relative;
  add options ~long_name:"only" ~help:"Output only the cluster(s) containing these packages (comma-separated)" c_only;
  add options ~long_name:"all-but" ~help:"Do not output the cluster(s) containing these packages (comma-separated)" c_but;
end

(* ----------------------------------- *)

module SG = Defaultgraphs.PackageGraph.G
module SO = Defaultgraphs.PackageGraph.O;;
module Dom = Dominators
module D = Defaultgraphs.PackageGraph.D;;
module PS = CudfAdd.Cudf_set

let is graph pkg = SG.fold_pred (fun p s -> PS.add p s) graph pkg (PS.singleton pkg) ;;

let scons graph pkg = SG.fold_succ (fun p s -> PS.add p s) graph pkg (PS.singleton pkg) ;;

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

let strongdeps_univ universe =
  let mdf = Mdf.load_from_universe universe in
  let g = Strongdeps_int.strongdeps_univ ~transitive:false mdf in
  Defaultgraphs.intcudf mdf.Mdf.index g
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose); 
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Algo.Dominators.dominators"; "Algo.Dominators.tarjan"; "Strongdeps_int.strong"; "Strongdeps_int.conjdep"]; 
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["Algo.dominators"]; 
  let (universe,_,_) = Boilerplate.load_universe posargs in

  let dom_graph =
    if OptParse.Opt.get Options.tarjan then
      Dom.dominators_tarjan (strongdeps_univ universe)
    else begin
      let g = Strongdeps.strongdeps_univ universe in
      if OptParse.Opt.get Options.do_cr then Dom.clique_reduction g;
      match OptParse.Opt.opt Options.relative with
      | None -> Dom.dominators g
      | Some f -> Dom.dominators ~relative:f g
    end
  in
  if OptParse.Opt.get Options.trans_red then SO.transitive_reduction dom_graph;
  begin 
    match OptParse.Opt.opt Options.c_only with
    | None -> ();
    | Some _ -> () 
  end; (* ???? *)
  if OptParse.Opt.get Options.do_clean then clean_graph dom_graph;
  if OptParse.Opt.is_set Options.out_file then
    D.output_graph (open_out (OptParse.Opt.get Options.out_file)) dom_graph
  else
    D.output_graph stdout dom_graph
;;

main () ;;
