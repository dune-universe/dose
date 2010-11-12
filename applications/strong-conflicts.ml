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

module Options = 
struct
  open OptParse

  let debug = StdOpt.store_true ()
  (* let strong = StdOpt.store_true ()
  let no_triangles = StdOpt.store_true () *)
  let log_file = StdOpt.str_option ()
  let out_file = StdOpt.str_option ()

  let description = "Compute list of strong conflicts"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~long_name:"log" ~help:"Use log file" log_file;
  add options ~long_name:"output" ~help:"Use output file" out_file;
  (* add options ~long_name:"strong" ~help:"Use strong dependencies" strong;
  add options ~long_name:"no-triangles" ~help:"Do not remove triangle conflicts" no_triangles; *)
end

let lc = ref None;;
let oc = ref stdout;;

let log s = 
begin
  match !lc with
  | None -> ()
  | Some l -> output_string l s
end;;

let _ =
(* let uri = ref "" in *)
begin
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug 3;
  (match OptParse.Opt.opt Options.log_file with
  | None -> lc := None
  | Some l -> lc := Some (open_out l));
  (match OptParse.Opt.opt Options.out_file with
  | None -> ()
  | Some l -> oc := open_out l);

  let (u, _, _) = Boilerplate.load_universe posargs in

  let sc = Strongconflicts.strongconflicts u in

  Strongconflicts.CG.iter_vertex (fun c1 ->
    let nc = Strongconflicts.CG.out_degree sc c1 in 
    Printf.fprintf !oc "%d %s:\n" nc (string_of_pkgname c1.package);
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
      Printf.fprintf !oc "  %d (%s <-> %s)\n" (List.length cl)
        (string_of_pkgname r1.package) (string_of_pkgname r2.package);
      List.iter (fun (c2, ct) -> 
        Printf.fprintf !oc "    * %s (%s)\n" (string_of_pkgname c2.package)
        (match ct with
        | Strongconflicts.Explicit -> "explicit"
        | Strongconflicts.Conjunctive -> "conjunctive"
        | Strongconflicts.Other _ -> "other")
      ) cl
    ) cf_ht
  ) sc
end;;
