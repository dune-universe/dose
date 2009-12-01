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
  let dot = ref false
  let dump = ref None
  let incr = ref false
  let detrans = ref false
end

let usage = Printf.sprintf "usage: %s [--options] doc" (Sys.argv.(0))
let options =
  [
   ("--dot", Arg.Set Options.dot, "Print the graph in dot format");
   ("--incr", Arg.Set Options.incr, "");
   ("--dump", Arg.String (fun f -> Options.dump := Some f ), "Dump the transitive reduction of the strong dependency graph");
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
    |("cudf",(_,_,_,_,file),_) ->
        let _, l, _ = CudfAdd.parse_cudf file in l
(* IFDEF HASRPM THEN *)
    |("hdlist",(_,_,_,_,file),_) -> begin
      let l = Rpm.Packages.Hdlists.input_raw [file] in
      let tables = Rpm.Rpmcudf.init_tables l in
      List.map (Rpm.Rpmcudf.tocudf tables) l
    end
    |("synth",(_,_,_,_,file),_) -> begin
      let l = Rpm.Packages.Synthesis.input_raw [file] in
      let tables = Rpm.Rpmcudf.init_tables l in
      List.map (Rpm.Rpmcudf.tocudf tables) l
    end
(* END *)
    |_ -> failwith "Not supported"

  in
  ignore(Common.Util.Timer.stop timer ());
  Printf.eprintf "done\n%!" ;
  pkglist
;;

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let files = ref [] in
  let _ =
    try Arg.parse options (fun f -> files := f::!files ) usage
    with Arg.Bad s -> failwith s
  in
  match !files with
  |[u] when !Options.incr = false ->
      let universe = Depsolver.trim (Cudf.load_universe (parse u)) in
      let sdgraph = Strongdeps.strongdeps_univ universe in
      Defaultgraphs.StrongDepGraph.out 
      ~dump:!Options.dump ~dot:!Options.dot ~detrans:!Options.detrans
      sdgraph
(*  |[newl;oldl;oldg] when !Options.incr = true ->
      begin
        let oldgraph = Defaultgraphs.StrongDepGraph.load oldg in
        let g = strong_incr (oldgraph,parse oldl) (parse newl) in
        Defaultgraphs.StrongDepGraph.out
        ~dump:!Options.dump ~dot:!Options.dot ~detrans:!Options.detrans
        g
      end
*)
  |_ -> (print_endline usage ; exit 2)

;;

main ();;

(* 
let version p =
  try (p.package,Cudf.lookup_package_property p "Number")
  with Not_found -> (p.package,string_of_int p.version)


(* this function can be more efficient if integrated in the library
 * and written using the low level strongdeps_int instead of 
 * the use level interface *)

let strong_incr (oldgraph,oldpkglist) newpkglist =
  let newuniv = Cudf.load_universe newpkglist in
  let oldh = CudfAdd.realversionmap oldpkglist in
  let newh = CudfAdd.realversionmap newpkglist in

  (* same     : packages that exists in old and new
   * changed  : packages that exists in new and have an older version in old
   * toremove : packages that exists in old but not in new
   * *)
  let (same,changed,toremove) =
    let same = ref [] in
    let changed = ref [] in
    let toremove = ref [] in
    Hashtbl.iter (fun (n,v) pkg ->
      if not(Hashtbl.mem oldh (n,v) ) then
        changed := pkg :: !changed
      else
        same := pkg :: !same
    ) newh;
    (* toremove : packages that do exist in old but not in new *)
    Hashtbl.iter (fun (n,v) pkg ->
      if not(Hashtbl.mem newh (n,v) ) then
        toremove := pkg :: !toremove
    ) oldh;
    (!same,!changed,!toremove)
  in

  (* print_endline "newpkglist----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p) ) newpkglist;

  print_endline "changed----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p) ) changed;
  print_endline "toremove----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p) ) toremove; *)

  (* the strong dependencies of a package p must be recomputed if either
   * closure p \inter changed = \neq \emptyset 
   * OR
   * conflicts (closure p) \inter changed = \neq \emptyset
   *)
  let torecompute =
    let module S = CudfAdd.Cudf_set in
    let maps = CudfAdd.build_maps newuniv in
    (* compute the list of all packages that conflict with a package
     * that was modified *)
    let sc = List.fold_right S.add changed S.empty in 
    let notsame =
      List.fold_left (fun acc pkg ->
        let l = maps.CudfAdd.who_conflicts pkg in
        let s = List.fold_right S.add l S.empty in
        if S.is_empty (S.inter s sc) then acc else (S.add pkg acc)
      ) S.empty changed
    in
    S.elements notsame
  in

  (*print_endline "torecompute----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p)) torecompute; *)

  (* the new list is the union of the dependency and reverse dependency closure
   * of all packages to recompute *)
  let newlist =
    let module S = CudfAdd.Cudf_set in
    let rl = Depsolver.reverse_dependency_closure newuniv torecompute in
    (* let dl = Depsolver.dependency_closure newuniv torecompute in *)
    let s = List.fold_right S.add changed S.empty in
    let s = List.fold_right S.add rl s in
    S.elements s
  in

  Printf.eprintf "same : %d\nchanged : %d\ntoremove : %d\ntorecompute : %d\nafterclosure : %d\n" 
  (List.length same)
  (List.length changed)
  (List.length toremove)
  (List.length torecompute)
  (List.length newlist)
  ;

  (*
  print_endline "newlist after closure----------------";
  List.iter (fun p -> print_endline (CudfAdd.print_package p) ) newlist;
  print_int (List.length newlist);
  print_newline ();
  print_endline "strong deps only for the new packages-----------------------------";
*)

  (* the graph of strong dependencies is computer considering a subset newlist
   * of packages of the new universe  *)
  let sdgraph = Strongdeps.strongdeps newuniv newlist in
  let newgraph = Defaultgraphs.StrongDepGraph.transform sdgraph in
  SO.transitive_reduction newgraph;
  (* out ~dot:true ~detrans:true newgraph;
  print_endline "-----------------------------";
  *)
  (* Cleanup : remove all old vertex and associated edges *)
  List.iter (fun pkg ->
    let p = version pkg in
    if SG.mem_vertex oldgraph p then begin
      SG.iter_succ (SG.remove_edge oldgraph p) oldgraph p ;
      SG.iter_pred (fun q -> SG.remove_edge oldgraph q p) oldgraph p; 
      SG.remove_vertex oldgraph p
    end
  ) toremove ;

  (* Cleanup : add all the new edges *)
  SG.iter_edges (fun p q -> SG.add_edge oldgraph p q) newgraph;

  oldgraph

;;

*)
