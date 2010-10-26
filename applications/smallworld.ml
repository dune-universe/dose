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

(* More info about small world networks
  http://en.wikipedia.org/wiki/Small-world_network *)

open Common

module Options = struct
  open OptParse
  let description = "Compute Small World statistic"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)
    
  let generic = StdOpt.store_true ()
  let scatterplots = StdOpt.store_true ()
  let connectivity = StdOpt.store_true ()
  let components = StdOpt.store_true ()
  let smallworld = StdOpt.store_true ()
  let centrality = StdOpt.store_true ()
  let strong_deps = StdOpt.store_true ()
  let combine_scatter = StdOpt.store_true ()
  let prefix = StdOpt.str_option ~default:"" ()

  open OptParser
  add options ~long_name:"prefix" ~help:"Prefix output fils with <prefix>" prefix;
  add options ~short_name:'g' ~long_name:"generic" ~help:"" generic;
  add options ~short_name:'p' ~long_name:"scatterplots" ~help:"" scatterplots;
  add options ~short_name:'c' ~long_name:"connectivity" ~help:"" connectivity;
  add options ~short_name:'m' ~long_name:"components" ~help:"" components;
  add options ~short_name:'s' ~long_name:"smallworld" ~help:"" smallworld;
  add options ~short_name:'e' ~long_name:"centrality" ~help:"" centrality;
  add options ~long_name:"strong-deps" ~help:"" strong_deps;
  add options ~long_name:"combine-scatter" ~help:"" combine_scatter;
end

let debug fmt = Util.make_debug "SmallWorld" fmt
let info fmt = Util.make_info "SmallWorld" fmt
let warning fmt = Util.make_warning "SmallWorld" fmt


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

let rec run outch = function
  |TestCase f -> ( f () ; Printf.fprintf outch "\n")
  |TestList l -> ( Printf.fprintf outch "\n"; List.iter (run outch) l )
  |TestLabel (l,t) -> ( Printf.fprintf outch "%s : " l ; (run outch) t )
;;

(**********************************)

module G = Defaultgraphs.PackageGraph.G
module S = Statistics.Make(G)

let saveplot h outfile =
  let out = open_out outfile in
  Printf.fprintf out "#count degree\n" ;
  Hashtbl.iter (fun n i -> Printf.fprintf out "%d %d\n" i n ) h;
  close_out out
;;

let saveplot2 h outfile =
  let out = open_out outfile in
  Printf.fprintf out "#count degree1 degree2\n" ;
  Hashtbl.iter (fun (n1, n2) i -> Printf.fprintf out "%d %d %d\n" i n1 n2) h;
  close_out out
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  let (universe,_,_) = Boilerplate.load_universe posargs in
  let gr = 
    if OptParse.Opt.get Options.strong_deps then
      Strongdeps.strongdeps_univ universe
    else 
      Defaultgraphs.PackageGraph.dependency_graph universe in
  let prefix = OptParse.Opt.get Options.prefix in
  let outch = if prefix = "" then stdout else open_out ( prefix ^ "stats" ) in
  let generic = "Generic" >::: [
    "Vertex" >:: (fun _ -> Printf.fprintf outch "%d" (G.nb_vertex gr));
    "Edges" >:: (fun _ -> Printf.fprintf outch "%d" (G.nb_edges gr));
    ]
  in
  let connectivity = "Connectivity" >::: [
    "Average Out-Degree" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageOutDegree gr));
    "Average In-Degree"  >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageInDegree gr));
    ]
  in
  let componentsSC = "Strongly Connected Components" >::: [
    "Number of Components SC"  >:: (fun _ -> Printf.fprintf outch "%d" (S.numberComponentsSC gr));
    "Average Components SC" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageComponentsSC gr));
    "Larges Component SC" >:: (fun _ -> Printf.fprintf outch "%d" (S.largestComponentSC gr));
    ]
  in
  let componentsWC = "Weakly Connected Components" >::: [
    "Number of Components WC"  >:: (fun _ -> Printf.fprintf outch "%d" (S.numberComponentsWC gr));
    "Average Components WC" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageComponentsWC gr));
    "Larges Component WC" >:: (fun _ -> Printf.fprintf outch "%d" (S.largestComponentWC gr));
    ]
  in
  let smallworld = "Small World" >::: [
    "Clustering Coefficient" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.clustering gr));
    "Average Shortest Path Length" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageShortestPathLength gr));
    "Density" >:: (fun _ -> Printf.fprintf outch "%0.5f" (S.density gr));
    "Average two step reach" >:: (fun _ -> Printf.fprintf outch "%0.2f" (S.averageTwoStepReach gr));
    ]
  in
  let centrality = "Centrality" >::: [
    "Centrality Out Degree" >:: (fun _ -> Printf.fprintf outch "%0.5f" (S.centralityOutDegree gr));
    "Centrality In Degree" >:: (fun _ -> Printf.fprintf outch "%0.5f" (S.centralityInDegree gr));
    ]
  in
  let scatterplots = "Scattered Plots" >::: [
    if OptParse.Opt.get Options.combine_scatter then
      ("Combined" >:: (fun _ ->
        saveplot2 (S.scatteredPlotBoth gr) (prefix^"degree.data"); print_string "Done";
      ))
    else
    ("Scattered Plot In" >:: (fun _ ->
      saveplot (S.scatteredPlotIn gr) (prefix^"indegree.data") ; print_string "Done" );
    "Scattered Plot Out" >:: (fun _ ->
      saveplot (S.scatteredPlotOut gr) (prefix^"outdegree.data"); print_string "Done" );
(*    "Hops Plot" >:: (fun _ ->
      saveplot (S.hopsplot gr 30) "hopsplot.data"; print_string "Done" );
      *) )
  ]
  in
  let t = ref [] in
  if not (OptParse.Opt.get Options.scatterplots) then t := scatterplots :: !t ;
  if not (OptParse.Opt.get Options.centrality) then t := centrality :: !t ;
  if not (OptParse.Opt.get Options.smallworld) then t := smallworld :: !t ;
  if not (OptParse.Opt.get Options.components) then t := componentsWC :: componentsSC :: !t ;
  if not (OptParse.Opt.get Options.connectivity) then t := connectivity :: !t ;
  if not (OptParse.Opt.get Options.generic) then t := generic :: !t ;
  run outch ("" >::: !t);
  close_out outch;
;;

main () ;;
