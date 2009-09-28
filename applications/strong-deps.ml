(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

open Cudf
open ExtLib
open Common
open Algo
open Graph

let enable_debug () = 
  (* enable the progress bar for strongdeps *)
  Common.Util.Progress.enable "Algo.Strongdep.main" ;
  Common.Util.Progress.enable "Algo.Strongdep.conj" ;
  Common.Util.set_verbosity Common.Util.Summary
;;

exception Done

module Options = struct
  let dot = ref false
  let dump = ref false
  let load = ref false
  let incr = ref false
  let detrans = ref false
end

let usage = Printf.sprintf "usage: %s [--options] doc" (Sys.argv.(0))
let options =
  [
   ("--dot", Arg.Set Options.dot, "Print the graph in dot format");
   ("--incr", Arg.Set Options.incr, "");
   ("--load", Arg.Set Options.incr, "Load a strong dependency graph");
   ("--dump", Arg.Set Options.dump, "Dump the transitive reduction of the strong dependency graph in graph.marshal");
   ("--detrans", Arg.Set Options.detrans, "Transitive reduction. Used in conjuction with --dot.");
   ("--debug", Arg.Unit enable_debug, "Print debug information");
  ]

(* ----------------------------------- *)

module G = Defaultgraphs.PackageGraph.G 
module D = Defaultgraphs.PackageGraph.D 
module O = Defaultgraphs.GraphOper(G)
module SG = Defaultgraphs.StrongDepGraph.G
module SD = Defaultgraphs.StrongDepGraph.D
module SO = Defaultgraphs.GraphOper(SG)

let version p =
  try (p.package,Cudf.lookup_package_property p "Number")
  with Not_found -> (p.package,string_of_int p.version)

let transform cudfgraph =
  let graph = SG.create () in
  G.iter_edges (fun p q ->
    let (np,vp) = version p in
    let (nq,vq) = version q in
    SG.add_edge graph (np,vp) (nq,vq)
  ) cudfgraph ;
  graph

let parse uri =
  Printf.eprintf "Parsing and normalizing...%!" ;
  let timer = Common.Util.Timer.create "Parsing and normalizing" in
  Common.Util.Timer.start timer;
  let pkglist =
    match Input.parse_uri uri with
    |("deb",(_,_,_,_,file),_) -> begin
      let l = Debian.Packages.input_raw [file] in
      let tables = Debian.Debcudf.init_tables l in
      List.map (Debian.Debcudf.tocudf tables) l
    end
    |("cudf",(_,_,_,_,file),_) -> begin
      let _, l, _ = CudfAdd.parse_cudf file in l
    end
    |_ -> failwith "Not supported"
  in
  ignore(Common.Util.Timer.stop timer ());
  Printf.eprintf "done\n%!" ;
  pkglist
;;

let out g =
  if !Options.dump then begin
    SO.transitive_reduction g;
    let oc = open_out "graph.marshal" in
    Marshal.to_channel oc (g :> SG.t) [];
    close_out oc
  end ;

  if !Options.dot then begin
    if !Options.detrans then
      SO.transitive_reduction g;
    SD.output_graph stdout g;
    print_newline ();
  end
;;

(* "graph.marshal" *)
let strong_incr (oldgraph,oldpkglist) newpkglist =
  let newuniv = Cudf.load_universe newpkglist in
  let oldh = CudfAdd.realversionmap oldpkglist in
  let newh = CudfAdd.realversionmap newpkglist in

  let (modified,toremove) =
    let nl = ref [] in
    let ol = ref [] in
    Hashtbl.iter (fun (n,v) pkg ->
      if not(Hashtbl.mem oldh (n,v) ) then
        nl := pkg :: !nl
    ) newh;
    Hashtbl.iter (fun (n,v) pkg ->
      if not(Hashtbl.mem newh (n,v) ) then
        ol := pkg :: !ol
    ) oldh;
    (!nl,!ol)
  in

  print_endline "modified----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p) ) modified;
  print_endline "toremove----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p) ) toremove;

  let newlist =
    let module S = CudfAdd.Cudf_set in
    let rl = Depsolver.reverse_dependency_closure newuniv modified in
    let dl = Depsolver.dependency_closure newuniv modified in
    let s = List.fold_right S.add dl S.empty in
    let s = List.fold_right S.add rl s in
    S.elements s
  in

  print_endline "newlist----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p) ) newlist;
  print_endline "newpkglist----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p) ) newpkglist;

  print_endline "strong deps only for the new packages-----------------------------";
  (* compute strong deps only for the new packages *)
  let newgraph = transform (Strongdeps.strongdeps newuniv newlist) in
  SO.transitive_reduction newgraph;
  out newgraph;
  print_endline "-----------------------------";

  (* add all the new edges *)
  SG.iter_edges (fun p q -> SG.add_edge oldgraph p q) newgraph;

  (* remove all old vertex and associated edges *)
  List.iter (fun pkg ->
    let p = version pkg in
    SG.iter_succ (SG.remove_edge oldgraph p) oldgraph p ;
    SG.iter_pred (fun q -> SG.remove_edge oldgraph q p) oldgraph p;
    SG.remove_vertex oldgraph p
  ) toremove ;

  print_endline "merge-----------------------------";
  oldgraph

;;

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let files = ref [] in
  let _ =
    try Arg.parse options (fun f -> files := f::!files ) usage
    with Arg.Bad s -> failwith s
  in
  match !files with
  |[f] when !Options.load = true -> failwith "not yet"
  |[f] when !Options.incr = false ->
      let universe = Cudf.load_universe (parse f) in
      out (transform (Strongdeps.strongdeps_univ universe))
  |[newl;oldl;oldg] when !Options.incr = true ->
      begin
        let ic = open_in oldg in 
        let oldgraph = ((Marshal.from_channel ic) :> SG.t) in 
        close_in ic ;
        let g = strong_incr (oldgraph,parse oldl) (parse newl) in
        out g
      end
  |_ -> (print_endline usage ; exit 2)

;;

main ();;
