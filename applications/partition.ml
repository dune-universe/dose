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

open Cudf
open ExtLib
open Common
open Algo
open Graph

let enable_debug () =
  (* enable the progress bars *)
  Common.Util.Progress.enable "Algo.Strongdep.main" ;
  Common.Util.Progress.enable "Algo.Strongdep.conj" ;
  Common.Util.set_verbosity Common.Util.Summary
;;

exception Done

module Options = struct
  let debug = ref false
end

let usage = Printf.sprintf "usage: %s [--options] doc" (Sys.argv.(0))
let options =
  [
   ("--debug", Arg.Unit enable_debug, "Print debug information");
  ]

(* ----------------------------------- *)

module PkgV = struct
    type t = int
    let compare = Pervasives.compare
    let hash i = i
    let equal = (=)
end
(* unlabelled indirected graph *)
module IG = Graph.Imperative.Graph.Concrete(PkgV)

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

let conflictgraph mdf =
  let index = mdf.Mdf.index in
  let g = IG.create () in
  for i=0 to (Array.length index - 1) do
    let pkg = index.(i) in
    let conflicts = Array.map snd pkg.Mdf.conflicts in
    for j=0 to ((Array.length conflicts) - 1) do
      IG.add_edge g i conflicts.(j)
    done
  done;
  g
;;

let connected_components g =
  let h = Hashtbl.create (IG.nb_vertex g) in
  let l = ref [] in
  let cc graph id =
    let module Dfs = Traverse.Dfs(IG) in
    let l = ref [] in
    let collect id = l := id :: !l in
    Dfs.prefix_component collect graph id;
    !l
  in
  IG.iter_vertex (fun v ->
    if not(Hashtbl.mem h v) then begin
      let c = cc g v in
      List.iter (fun x -> Hashtbl.add h x ()) c ;
      l := c :: !l
    end
  ) g ;
  !l
;;

let cmp l1 l2 = (List.length l2) - (List.length l1)

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let files = ref [] in
  let _ =
    try Arg.parse options (fun f -> files := f::!files ) usage
    with Arg.Bad s -> failwith s
  in
  match !files with
  |[f] ->
      let pkglist = parse f in
      let mdf = Mdf.load_from_list pkglist in
      let cg = conflictgraph mdf in
      let cc = connected_components cg in
      Printf.eprintf "conflict graph = vertex : %d , edges : %d\n"
      (IG.nb_vertex cg)(IG.nb_edges cg);
      Printf.eprintf "connected components = n. %d , largest : %d\n"
      (List.length cc) (List.length (List.hd (List.sort ~cmp:cmp cc)));
      ()
      (* conflict graph,
       * connected components,
       * ...
       *)
  |_ -> (print_endline usage ; exit 2)
;;

main ();;
