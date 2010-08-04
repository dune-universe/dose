(**************************************************************************************)
(*  Copyright (c) 2009-2010 Jaap Boender                                              *)
(*  Copyright (C) 2009-2010 Mancoosi Project                                          *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(* attempt at computation of strong conflicts with dose3 (TLFKAlibmancoosi) *)

open Common
open ExtLib
open Cudf
open Cudf_types_pp
open Diagnostic

(* module Graph = Defaultgraphs.SyntacticDependencyGraph
module G = Graph.G
module V = Graph.PkgV
module E = Graph.PkgE
module SG = Defaultgraphs.PackageGraph.G *)

let enable_debug () =
    (* enable the progress bars *)
    Common.Util.Progress.enable "Algo.Strongdep.main" ;
    Common.Util.Progress.enable "SyntacticDependencyGraph.dependency_graph" ;
    Common.Util.set_verbosity Common.Util.Details
;;


let usage = Printf.sprintf "usage: %s [--debug] [--log file] uri" Sys.argv.(0);;
let logfile = ref (open_out "/dev/null");;
let use_strong_conflicts = ref false;;
let remove_triangles = ref true;;
let oc = ref stdout;;

let options = [
  ("--debug", Arg.Unit enable_debug, "Print debug information");
  ("--log", Arg.String (fun s -> close_out !logfile; logfile := open_out s), "Dump log information in file");
  ("--strong", Arg.Set use_strong_conflicts, "Use strong conflicts");
  ("--no-triangles", Arg.Clear remove_triangles, "Do not remove triangles");
  ("--output", Arg.String (fun s -> oc := open_out s), "Use this file for output");
];;

let log s = 
begin
  output_string !logfile s;
  flush !logfile
end;;

let _ =
let uri = ref "" in
begin
  at_exit (fun () -> Util.dump Format.err_formatter);
  let _ =
    try Arg.parse options (fun f -> uri := f) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then
  begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  Printf.eprintf "Parsing...%!";
  (* let timer = Util.Timer.create "Parsing" in
  Util.Timer.start timer; *)
  let u = match Input.parse_uri !uri with
  | ("deb", (_,_,_,_,file),_) ->
    begin
      let l = Debian.Packages.input_raw [file] in
      Debian.Debcudf.load_universe l
    end
  | ("hdlist", (_,_,_,_,file),_) ->
    begin
      let l = Rpm.Packages.Hdlists.input_raw [file] in
      Rpm.Rpmcudf.load_universe l
    end
  | ("synth", (_,_,_,_,file),_) ->
    begin
      let l = Rpm.Packages.Synthesis.input_raw [file] in
      Rpm.Rpmcudf.load_universe l
    end
  | ("cudf",  (_,_,_,_,file),_) ->
      begin
	let cudf_load_list file =
	  let _, pkglist, _ = Boilerplate.parse_cudf file in
	  let from_cudf pkg = (pkg.Cudf.package,string_of_int pkg.Cudf.version) in
	  let to_cudf (p,v) = (p,int_of_string v) in
	  (pkglist,from_cudf,to_cudf)
	in

	let cudf_load_universe file =
	  let (l,f,t) = cudf_load_list file in
	  (Cudf.load_universe l, f, t)
	in
	let (u,_,_) = cudf_load_universe file in u
      end
  | (s, _, _) -> failwith (Printf.sprintf "%s: not supported\n" s) in
  (* ignore (Util.Timer.stop timer ()); *)
  Printf.eprintf "done\n%!";

  let sc = Strongconflicts.strongconflicts u in

  Strongconflicts.CG.iter_vertex (fun c1 ->
    let nc = Strongconflicts.CG.out_degree sc c1 in 
    Printf.printf "%d %s:\n" nc (string_of_pkgname c1.package);
    let cf_ht = Hashtbl.create nc in 
    Strongconflicts.CG.iter_succ_e (fun (_, (r1, r2, ct), c2) ->
      (* aggregate strong conflicts by root *)
      try
        let cl = Hashtbl.find cf_ht (r1, r2) in
        Hashtbl.replace cf_ht (r1, r2) ((c2, ct)::cl)
      with Not_found ->
        Hashtbl.add cf_ht (r1, r2) [c2, ct];
    ) sc c1;
    Hashtbl.iter (fun (r1, r2) cl ->
      Printf.printf "  %d (%s <-> %s)\n" (List.length cl)
        (string_of_pkgname r1.package) (string_of_pkgname r2.package);
      List.iter (fun (c2, ct) -> 
        Printf.printf "    * %s (%s)\n" (string_of_pkgname c2.package)
        (match ct with
        | Strongconflicts.Explicit -> "explicit"
        | Strongconflicts.Conjunctive -> "conjunctive"
        | Strongconflicts.Other _ -> "other")
      ) cl
    ) cf_ht
  ) sc
end;;
