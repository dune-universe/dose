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

module Options =
struct
  open OptParse
  let debug = StdOpt.store_true ()

  let description = "Ceve ... what does it mean ?"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~long_name:"debug" ~help:"Print debug information" debug;
end

(* XXX to refactor in Borilerplate.ml *)
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

(* ----------------------------------- *)

module PkgV = struct
    type t = int
    let compare = Pervasives.compare
    let hash i = i
    let equal = (=)
end
(* unlabelled indirected graph *)
module IG = Graph.Imperative.Graph.Concrete(PkgV)

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

(* associate a connected component to each conflict node *)
let conflict_table cc =
  let h = Hashtbl.create (2 * List.length cc) in
  List.iter (fun l ->
    List.iter (fun v ->
      Hashtbl.add h v (ref l)
    ) l
  ) cc
  ;
  h
;;

(* associate a list of connected components to each package *)
let package_table reverse cg ct =
  let h = Hashtbl.create (IG.nb_vertex cg) in
  let rev_clo pid = Depsolver_int.reverse_dependency_closure reverse [pid] in
  IG.iter_vertex (fun v ->
    List.iter (fun p ->
      try
        let l = Hashtbl.find h p in
        try l := (Hashtbl.find ct v)::!l
        with Not_found -> assert false
      with Not_found -> Hashtbl.add h p (ref [])
    ) (rev_clo v)
  ) cg
  ;
  h
;;

let cmp l1 l2 = (List.length l2) - (List.length l1)

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  let pkglist = match posargs with [uri] -> parse uri | _ -> assert false in
  let mdf = Mdf.load_from_list pkglist in
  let maps = mdf.Mdf.maps in
  let reverse = Depsolver_int.reverse_dependencies mdf in
  let cg = conflictgraph mdf in
  let cc = connected_components cg in
  Printf.eprintf "conflict graph = vertex : %d , edges : %d\n"
  (IG.nb_vertex cg)(IG.nb_edges cg);
  Printf.eprintf "connected components = n. %d , largest : %d\n"
  (List.length cc) (List.length (List.hd (List.sort ~cmp:cmp cc)));
  let ct = conflict_table cc in
  let pt = package_table reverse cg ct in
  let maxc = ref 0 in
  let sumc = ref 0 in
  Hashtbl.iter (fun p l ->
    let m = (List.length !l) in
    if m > !maxc then maxc := m;
    sumc := m + !sumc;
    Printf.eprintf "%s : %d\n" (CudfAdd.print_package (maps.CudfAdd.map#inttovar p)) m
  ) pt
  ;
  Printf.eprintf "max = %d , avg = %d\n" !maxc (!sumc / (Hashtbl.length pt))
  (* conflict graph,
   * connected components,
   * ...
   *)
;;

main ();;
