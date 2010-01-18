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
open Sql

module Options =
struct
  open OptParse
  let debug = StdOpt.store_true ()
  let src = StdOpt.str_option ()
  let dst = StdOpt.str_option ()
  let cone = StdOpt.str_option ()
  let reverse_cone = StdOpt.str_option ()
  let cone_maxdepth = StdOpt.int_option ()
  let output_ty = StdOpt.str_option ~default:"cnf" ()
  let out_ch = StdOpt.str_option ()
  (* type output_types = Dot | CNF | Dimacs | PrettyPrint | SQLite *)

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
  add options ~short_name:'t' ~long_name:"outtype" ~help:"Output type : dot | cnf | dimacs | pp" output_ty;
  add options ~short_name:'o' ~long_name:"outfile" ~help:"Output file" out_ch;
end;;

let and_sep_re = Str.regexp "\\s*;\\s*"
let pkg_re = Str.regexp "(\\([a-z][a-z0-9.+-]+\\)\\s*,\\s*\\([a-zA-Z0-9.+:~-]+\\))"
let parse_pkg s =
  let parse_aux str =
    if Str.string_match pkg_re str 0 then begin
      (Str.matched_group 1 str, Str.matched_group 2 str)
    end
    else
      (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Str.split and_sep_re s);;

(* -------------------------------- *)

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
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

  let output_to_sqlite univ =
    let db = Backend.open_database "sqlite" (None, None, Some "localhost", None, "cudf") in
    let enr = !Sql.database.exec_no_result db.Backend.connection in
    let add_package p =
    begin
      enr (Printf.sprintf "INSERT INTO version (number, name) VALUES ('%s', '%s')" (lookup_package_property p "number") p.package)
    end in
  begin
      enr
        "CREATE TABLE IF NOT EXISTS file (
           sha1sum VARCHAR PRIMARY KEY UNIQUE,
           timestamp DATESTAMP,
           path VARCHAR
         )";
      enr  
        "CREATE TABLE IF NOT EXISTS aptlist (
           id INTEGER PRIMARY KEY,
           timestamp DATESTAMP,
           processed BOOLEAN,
           sha1sum VARCHAR,
           sha1base VARCHAR,
           packages_id INTEGER,
           file_sha1sum INTEGER UNIQUE,
           FOREIGN KEY (file_sha1sum) REFERENCES file(sha1sum)
         )";
      enr
        "CREATE TABLE IF NOT EXISTS packages (
           id INTEGER PRIMARY KEY,
           aptlist_id VARCHAR UNIQUE,
           mirror VARCHAR,
           arch VARCHAR,
           comp VARCHAR,
           suite VARCHAR,
           FOREIGN KEY (aptlist_id) REFERENCES aptlist(id)
         )";
      enr
        "CREATE TABLE IF NOT EXISTS info (
           id INTEGER PRIMARY KEY,
           essential BOOLEAN,
           priority VARCHAR,
           section VARCHAR,
           installed_size INT,
           maintainer VARCHAR,
           architecture VARCHAR,
           source VARCHAR,
           package_size INT,
           build_essential BOOLEAN
         )";
      enr
        "CREATE TABLE IF NOT EXISTS interval (
           version_id INT,
           aptlist_id INT,
           PRIMARY KEY (version_id,aptlist_id),
           FOREIGN KEY (version_id) REFERENCES version(id),
           FOREIGN KEY (aptlist_id) REFERENCES aptlist(id),
           UNIQUE (version_id, aptlist_id)
         )";
      enr
        "CREATE TABLE IF NOT EXISTS version (
           id INTEGER PRIMARY KEY,
           number VARCHAR COLLATE DEBIAN,
           name VARCHAR,
           unit_id INT,
           info_id INT,
           replaces VARCHAR,
           provides VARCHAR,
           pre_depends VARCHAR,
           depends VARCHAR,
           suggests VARCHAR,
           enhances VARCHAR,
           recommends VARCHAR,
           conflicts VARCHAR,
           FOREIGN KEY (info_id) REFERENCES info(id),
           UNIQUE (unit_id, number)
         )";
    Cudf.iter_packages (fun p ->
      add_package p
    ) univ;
    !Sql.database.close_db db.Backend.connection;
  end in

  let u = Cudf.load_universe plist in
  match OptParse.Opt.get Options.output_ty with
  |"dot" -> 
IFDEF HAS_OCAMLGRAPH THEN
      let module Graph = Defaultgraphs.SyntacticDependencyGraph in
      Graph.D.output_graph (Options.output_ch ()) (Graph.dependency_graph u)
ELSE
    failwith ("dot Not supported")
END
  |"cnf" -> Printf.fprintf (Options.output_ch ()) "%s" (Depsolver.output_clauses u)
  |"dimacs" -> Printf.fprintf (Options.output_ch ()) "%s" (Depsolver.output_clauses ~enc:Depsolver.Dimacs u)
  |"pp" -> Printf.fprintf (Options.output_ch ()) "%s" (Cudf_printer.string_of_universe u)
  |"sqlite" -> output_to_sqlite u
  |s -> (Printf.eprintf "unknown format : %s" s ; exit 1)
;;

main ();;
