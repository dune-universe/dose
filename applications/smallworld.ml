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

(* Copyright (C) 2008 Pietro Abate <pietro.abate@pps.jussieu.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * More info about small world networks
 * http://en.wikipedia.org/wiki/Small-world_network
 *)

open Common

module Options =
  struct
    open OptParse
    let debug = StdOpt.store_true ()
    let generic = StdOpt.store_true ()
    let scatterplots = StdOpt.store_true ()
    let connectivity = StdOpt.store_true ()
    let components = StdOpt.store_true ()
    let smallworld = StdOpt.store_true ()
    let centrality = StdOpt.store_true ()
    let outdir = StdOpt.str_option ()

    let options = OptParser.make ()
    open OptParser

    add options ~short_name:'d' ~long_name:"debug" ~help:"Print various aggregate information" debug;
    add options ~long_name:"outdir" ~help:"Send output to a file" outdir;
    add options ~short_name:'g' ~long_name:"generic" ~help:"" generic;
    add options ~short_name:'p' ~long_name:"scatterplots" ~help:"" scatterplots;
    add options ~short_name:'c' ~long_name:"connectivity" ~help:"" connectivity;
    add options ~short_name:'m' ~long_name:"components" ~help:"" components;
    add options ~short_name:'s' ~long_name:"smallworld" ~help:"" smallworld;
    add options ~short_name:'e' ~long_name:"centrality" ~help:"" centrality;
  end

(**********************************)

(* ounit anybody :) *)
type test_fun = unit -> unit
type test =
  |TestCase of test_fun
  |TestList of test list
  |TestLabel of string * test

let (>:) s t = TestLabel(s,t)
let (>::) s f = TestLabel(s, TestCase(f))
let (>:::) s l = TestLabel(s, TestList(l))

let print_test s f = (fun _ -> Printf.printf s f ; ())

let rec run = function
  |TestCase f -> ( f () ; print_newline ())
  |TestList l -> ( print_newline () ; List.iter run l )
  |TestLabel (l,t) -> ( Printf.printf "%s : " l ; run t )

(**********************************)

module G = Defaultgraphs.SyntacticDependencyGraph.G
module S = Statistics.Make(G)

let saveplot h outfile =
  let out = open_out outfile in
  Printf.fprintf out "#count degree\n" ;
  Hashtbl.iter (fun n i -> Printf.fprintf out "%d %d\n" i n ) h;
  close_out out
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  let (universe,_,_) = Boilerplate.load_universe posargs in
  let gr = Defaultgraphs.SyntacticDependencyGraph.dependency_graph universe in
  let generic = "Generic" >::: [
    "Vertex" >:: (fun _ -> Printf.printf "%d" (G.nb_vertex gr));
    "Edges" >:: (fun _ -> Printf.printf "%d" (G.nb_edges gr));
    ]
  in
  let connectivity = "Connectivity" >::: [
    "Average Out-Degree" >:: (fun _ -> Printf.printf "%0.2f" (S.averageOutDegree gr));
    "Average In-Degree"  >:: (fun _ -> Printf.printf "%0.2f" (S.averageInDegree gr));
    ]
  in
  let componentsSC = "Strongly Connected Components" >::: [
    "Number of Components SC"  >:: (fun _ -> Printf.printf "%d" (S.numberComponentsSC gr));
    "Average Components SC" >:: (fun _ -> Printf.printf "%0.2f" (S.averageComponentsSC gr));
    "Larges Component SC" >:: (fun _ -> Printf.printf "%d" (S.largestComponentSC gr));
    ]
  in
  let componentsWC = "Weakly Connected Components" >::: [
    "Number of Components WC"  >:: (fun _ -> Printf.printf "%d" (S.numberComponentsWC gr));
    "Average Components WC" >:: (fun _ -> Printf.printf "%0.2f" (S.averageComponentsWC gr));
    "Larges Component WC" >:: (fun _ -> Printf.printf "%d" (S.largestComponentWC gr));
    ]
  in
  let smallworld = "Small World" >::: [
    "Clustering Coefficient" >:: (fun _ -> Printf.printf "%0.2f" (S.clustering gr));
    "Average Shortest Path Length" >:: (fun _ -> Printf.printf "%0.2f" (S.averageShortestPathLength gr));
    "Density" >:: (fun _ -> Printf.printf "%0.5f" (S.density gr));
    "Average two step reach" >:: (fun _ -> Printf.printf "%0.2f" (S.averageTwoStepReach gr));
    ]
  in
  let centrality = "Centrality" >::: [
    "Centrality Out Degree" >:: (fun _ -> Printf.printf "%0.5f" (S.centralityOutDegree gr));
    "Centrality In Degree" >:: (fun _ -> Printf.printf "%0.5f" (S.centralityInDegree gr));
    ]
  in
  let scatterplots = "Scattered Plots" >::: [
    "Scattered Plot In" >:: (fun _ ->
      saveplot (S.scatteredPlotIn gr) "indegree.data" ; print_string "Done" );
    "Scattered Plot Out" >:: (fun _ ->
      saveplot (S.scatteredPlotOut gr) "outdegree.data"; print_string "Done" );
(*    "Hops Plot" >:: (fun _ ->
      saveplot (S.hopsplot gr 30) "hopsplot.data"; print_string "Done" );
      *)
  ]
  in
  let t = ref [] in
  if not (OptParse.Opt.get Options.scatterplots) then t := scatterplots :: !t ;
  if not (OptParse.Opt.get Options.centrality) then t := centrality :: !t ;
  if not (OptParse.Opt.get Options.smallworld) then t := smallworld :: !t ;
  if not (OptParse.Opt.get Options.components) then t := componentsWC :: componentsSC :: !t ;
  if not (OptParse.Opt.get Options.connectivity) then t := connectivity :: !t ;
  if not (OptParse.Opt.get Options.generic) then t := generic :: !t ;
  run ("" >::: !t)
;;

main () ;;
