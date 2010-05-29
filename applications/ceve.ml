(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  and Jaap Boender <boender@pps.jussieu.fr *)
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

IFDEF HASOCAMLGRAPH THEN
  module DGraph = Defaultgraphs.SyntacticDependencyGraph
END

module Options =
struct
  open OptParse

  exception Format
  let out_option ?default ?(metavar = "<dot|cnf|dimacs|pp>") () =
    let corce = function
      |("cnf"|"dimacs"|"pp"|"dot") as s -> s
      | _ -> raise Format
    in
    let error _ s = Printf.sprintf "%s format not supported" s in
    Opt.value_option metavar default corce error

  let debug = StdOpt.store_true ()
  let src = StdOpt.str_option ()
  let dst = StdOpt.str_option ()
  let cone = StdOpt.str_option ()
  let reverse_cone = StdOpt.str_option ()
  let cone_maxdepth = StdOpt.int_option ()
  let output_ty = out_option ~default:"cnf" ()
  let out_ch = StdOpt.str_option ()

  let output_ch () =
    if Opt.is_set out_ch then 
      open_out (Opt.get out_ch)
    else stdout

  let description = "Ceve ... what does it mean ?"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options                 ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'s' ~long_name:"src" ~help:"root packages" src;
  add options ~short_name:'d' ~long_name:"dst" ~help:"pivot packages" dst;
  add options ~short_name:'c' ~long_name:"cone" ~help:"cone" cone;
  add options ~short_name:'r' ~long_name:"rcone" ~help:"reverse dependency cone" reverse_cone;
  add options                 ~long_name:"depth" ~help:"max depth - in conjunction with cone" cone_maxdepth;
  add options ~short_name:'t' ~long_name:"outtype" ~help:"Output type" output_ty;
  add options ~short_name:'o' ~long_name:"outfile" ~help:"Output file" out_ch;
end;;

let and_sep_re = Pcre.regexp "\\s*;\\s*"
let pkg_re = Pcre.regexp "\\(([a-z][a-z0-9.+-]*)\\s*,\\s*([a-zA-Z0-9.+:~-]+)\\)"
let parse_pkg s =
  let parse_aux str =
    try 
      let s = Pcre.exec ~rex:pkg_re str  in
      (Pcre.get_substring s 1, Pcre.get_substring s 2)
    with
      Not_found -> (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Pcre.split ~rex:and_sep_re s);;

(* -------------------------------- *)

let output_to_sqlite args =
IFDEF HASDB THEN
  begin
    let pl = List.unique (List.flatten (List.map (function u ->
      match Input.parse_uri u with
      | ("deb", (_, _, _, _, f), _) -> Debian.Packages.input_raw [f]
      | _ -> failwith "Other file formats than Debian are not yet supported for SQLite insertion"
     ) args)) in
    let db = Backend.open_database "sqlite" (None, None, Some "localhost", None, "cudf") in
    Backend.create_tables db; 
    List.iter (fun p ->
      Backend.insert_package db p
    ) pl;
    (* !Sql.database.close_db db.Backend.connection; *)
  end 
ELSE
  failwith "DB not available"
END
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  if OptParse.Opt.get Options.output_ty = "sqlite" then
    output_to_sqlite posargs
  else
  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  let get_cudfpkg (p,v) = Cudf.lookup_package universe (to_cudf (p,v)) in

  let pkg_src () = List.map get_cudfpkg (parse_pkg (OptParse.Opt.get Options.src)) in
  let pkg_dst () =
    (* all packages q in R s.t. q is in the dependency closure of p *)
    let (p,v) = List.hd(parse_pkg (OptParse.Opt.get Options.dst)) in
    let pid = get_cudfpkg (p,v) in
    List.filter_map (fun pkg ->
      if List.mem pid (Depsolver.dependency_closure universe [pkg]) then
        Some(pkg)
      else None
    ) (Cudf.get_packages universe) 
  in
  let pkg_cone () =
    List.unique (List.fold_left (fun acc (p,v) ->
      let pid = get_cudfpkg (p,v) in
      if OptParse.Opt.is_set Options.cone_maxdepth then
        let md = OptParse.Opt.get Options.cone_maxdepth in
        (Depsolver.dependency_closure ~maxdepth:md universe [pid]) @ acc
      else
        (Depsolver.dependency_closure universe [pid]) @ acc
    ) [] (parse_pkg (OptParse.Opt.get Options.cone)))
  in
  let pkg_reverse_cone () =
    List.unique (List.fold_left (fun acc (p,v) ->
      let pid = get_cudfpkg (p,v) in
      if OptParse.Opt.is_set Options.cone_maxdepth then
        let md = OptParse.Opt.get Options.cone_maxdepth in
        (Depsolver.reverse_dependency_closure ~maxdepth:md universe [pid]) @ acc
      else
        (Depsolver.reverse_dependency_closure universe [pid]) @ acc
    ) [] (parse_pkg (OptParse.Opt.get Options.reverse_cone)))
  in

  let pkg_src_list = ref [] in
  let pkg_dst_list = ref [] in
  let plist =
    if OptParse.Opt.is_set Options.src && OptParse.Opt.is_set Options.dst then begin
      let (p,v) = List.hd(parse_pkg (OptParse.Opt.get Options.dst)) in
      let pid = get_cudfpkg (p,v) in
      pkg_src_list := pkg_src ();
      pkg_dst_list := [pid];
      (pid::!pkg_src_list)
    end
    else if OptParse.Opt.is_set Options.src then begin
      pkg_src_list := pkg_src ();
      !pkg_src_list
    end
    else if OptParse.Opt.is_set Options.dst then begin
      pkg_dst_list := pkg_dst ();
      !pkg_dst_list
    end
    else if OptParse.Opt.is_set Options.cone then 
      pkg_cone ()
    else if OptParse.Opt.is_set Options.reverse_cone then
      pkg_reverse_cone ()
    else Cudf.get_packages universe
  in

  let output ll =
    List.iter (fun l ->
      let u = Cudf.load_universe l in
      let oc = Options.output_ch () in
      begin match OptParse.Opt.get Options.output_ty with
      |"dot" -> 
IFDEF HASOCAMLGRAPH THEN
          DGraph.D.output_graph oc (DGraph.dependency_graph u)
ELSE
        failwith ("dot Not supported")
END
      |"cnf" -> Printf.fprintf oc "%s" (Depsolver.output_clauses ~enc:Depsolver.Cnf u)
      |"dimacs" -> Printf.fprintf oc "%s" (Depsolver.output_clauses ~enc:Depsolver.Dimacs u)
      |"pp" -> Cudf_printer.pp_universe (Format.formatter_of_out_channel oc) universe
      |_ -> assert false
      end ;
      close_out oc;
    ) ll
  in output [plist]
;;

main ();;
