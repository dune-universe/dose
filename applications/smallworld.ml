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


open Graph

module Options = struct
  let debug = ref 0
  let generic = ref false
  let scatterplots = ref false
  let connectivity = ref false 
  let components = ref false
  let smallworld = ref false
  let centrality = ref false
end

let usage = Printf.sprintf "usage: %s [-options] [dot graph]" (Sys.argv.(0))
let options =
  [
(*   ("-d", Arg.Int (fun i -> Options.debug := i), "Turn on debugging info level"); *)
   ("--no-generic", Arg.Set Options.generic, "");
   ("--no-scatterplots", Arg.Set Options.scatterplots, "");
   ("--no-connectivity", Arg.Set Options.connectivity, "");
   ("--no-components", Arg.Set Options.components, "");
   ("--no-smallworld", Arg.Set Options.smallworld, "");
   ("--no-centrality", Arg.Set Options.centrality, "");
  ]

let input_file = ref None 
let file f = input_file := Some(f)

(**********************************)

module PkgV = struct
    type t = string
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal l1 l2 = (l1 = l2)
end

module G = Imperative.Digraph.ConcreteBidirectional(PkgV) 
module B = Builder.I(G)
module L = struct
  open Dot_ast
  let node (id,_) attrs =
    let str = function | Ident s | Number s | String s | Html s -> s in
    str id
  let edge l = ()
end

module DIn = Dot.Parse (B)(L)

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

module S = Statistics.Make(G)

let saveplot h outfile =
  let out = open_out outfile in
  Printf.fprintf out "#count degree\n" ;
  Hashtbl.iter (fun n i -> Printf.fprintf out "%d %d\n" i n ) h;
  close_out out
;;

let main () = 
  let _ =
    try Arg.parse options file usage
    with Arg.Bad s -> failwith s
  in
  let gr =
    match !input_file with
    |None -> (print_endline usage ; exit 1)
    |Some f -> DIn.parse f
  in
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
  if not !Options.scatterplots then t := scatterplots :: !t ;
  if not !Options.centrality then t := centrality :: !t ;
  if not !Options.smallworld then t := smallworld :: !t ;
  if not !Options.components then t := componentsWC :: componentsSC :: !t ;
  if not !Options.connectivity then t := connectivity :: !t ;
  if not !Options.generic then t := generic :: !t ;
  run ("" >::: !t)
;;

main () ;;
