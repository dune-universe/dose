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
open Graph

let enable_debug () = 
  (* enable the progress bar for strongdeps *)
  Common.Util.Progress.enable "Algo.Strongconflicts.local" ;
  Common.Util.Progress.enable "Algo.Strongconflicts.seeding" ;
  Common.Util.set_verbosity Common.Util.Summary
;;

exception Done

module Options = struct
  let debug = ref false
end

let usage = Printf.sprintf "usage: %s [--options] [strong deps graph] doc" (Sys.argv.(0))
let options =
  [
   ("--debug", Arg.Unit enable_debug, "Print debug information");
  ]

(* ----------------------------------- *)

module G = Defaultgraphs.PackageGraph.G 
module D = Defaultgraphs.PackageGraph.D 
module O = Defaultgraphs.GraphOper(G)
module SG = Defaultgraphs.StrongDepGraph.G
module SD = Defaultgraphs.StrongDepGraph.D
module SO = Defaultgraphs.GraphOper(G)


let transform pkglist graph =
  let vermap = CudfAdd.realversionmap pkglist in
  let package vermap (n,v) =
      try Hashtbl.find vermap (n,v) with Not_found -> assert false
  in
  let cudfgraph = G.create () in
  SG.iter_edges (fun p q ->
    let x = package vermap p in
    let y = package vermap q in
    G.add_edge cudfgraph x y
  ) graph ;
  cudfgraph

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

open CudfAdd

let soundness universe l =
  let solver = Depsolver.load universe in
  List.iter (fun (p,q) ->
    let d = Depsolver.edos_coinstall solver [p;q] in
    match d.Diagnostic.result with
    |Diagnostic.Success _ -> failwith "Unsound"
    |Diagnostic.Failure _ -> ()
  ) l
  ;
;;

let swap (p,q) = if p.Cudf.package < q.Cudf.package then (p,q) else (q,p)

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let files = ref [] in
  let _ =
    try Arg.parse options (fun f -> files := f::!files ) usage
    with Arg.Bad s -> failwith s
  in
  match !files with
  |[u] ->
      let pkglist = parse u in
      let universe = Cudf.load_universe pkglist in
      Common.Util.print_info "Computing Strong Dependencies";
      (* let tgraph = Strongdeps.strongdeps_univ universe in *)
      let tgraph = SO.O.add_transitive_closure (Strongdeps.conjdeps universe) in
      Common.Util.print_info "done";

      let l = Strongconflicts.strongconflicts tgraph universe pkglist in
      Common.Util.print_info "Soundness test" ;
      soundness universe l;
      Common.Util.print_info "done"; 

      List.iter (fun (x,y) ->
        let (x,y) = swap (x,y) in
        Printf.printf "%s <-> %s\n" (CudfAdd.print_package x) (CudfAdd.print_package y)
      ) l
      ;
      Common.Util.print_info "Total strong conflicts %d" (List.length l)
  |[u;g] ->
      Common.Util.print_info "Load Strong Dependencies graph";
      let ic = open_in g in 
      let graph = ((Marshal.from_channel ic) :> SG.t) in 
      close_in ic ;
      let pkglist = parse u in
      let tg = transform pkglist graph in
      Common.Util.print_info "done";

      Common.Util.print_info "Compute transitive closusure of the SD graph";
      let tgraph = SO.O.add_transitive_closure tg in
      Common.Util.print_info "done";

      let universe = Cudf.load_universe pkglist in
      let l = Strongconflicts.strongconflicts tgraph universe pkglist in

      Common.Util.print_info "Soundness test " ;
      soundness universe l;
      Common.Util.print_info "done" ;

      List.iter (fun (x,y) ->
        let (x,y) = swap (x,y) in
        Printf.printf "%s <-> %s\n" (CudfAdd.print_package x) (CudfAdd.print_package y)
      ) l

      ;
      Common.Util.print_info "Total strong conflicts %d" (List.length l)
  |_ -> (print_endline usage ; exit 2)

;;

main ();;
