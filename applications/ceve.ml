(**************************************************************************************)
(*  Copyright (C) 2009,2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                *)
(*                          and Jaap Boender <boender@pps.jussieu.fr>                 *)
(*  Copyright (C) 2009,2010 Mancoosi Project                                          *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Common
open Algo

include Util.Logging(struct let label = __FILE__ end) ;;

IFDEF HASOCAMLGRAPH THEN
  module DGraph = Defaultgraphs.SyntacticDependencyGraph
END

module Options = struct
  open OptParse
  let description = "Ceve - integrated metadata parser and transformer"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  exception Format
  let out_option ?default ?(metavar = "<dot|cnf|dimacs|cudf|table>") () =
    let corce = function
      |("cnf"|"dimacs"|"cudf"|"dot"|"table") as s -> s
      | _ -> raise Format
    in
    let error _ s = Printf.sprintf "%s format not supported" s in
    Opt.value_option metavar default corce error

  let src = StdOpt.str_option ()
  let dst = StdOpt.str_option ()
  let cone = StdOpt.str_option ()
  let extract = StdOpt.str_option ()
  let reverse_cone = StdOpt.str_option ()
  let cone_maxdepth = StdOpt.int_option ()
  let out_type = out_option ~default:"cnf" ()
  let out_file = StdOpt.str_option ()

  let deb_foreign_arch = Boilerplate.str_list_option ()
  let deb_native_arch = StdOpt.str_option ()
  let deb_host_arch = StdOpt.str_option ()
  let deb_build_arch = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'e' ~long_name:"extract" ~help:"dependency/conflict cone" extract;
  add options ~short_name:'c' ~long_name:"cone" ~help:"dependency cone" cone;
  add options ~short_name:'r' ~long_name:"rcone" ~help:"reverse dependency cone" reverse_cone;
  add options                 ~long_name:"depth" ~help:"max depth - in conjunction with cone" cone_maxdepth;
  add options ~short_name:'t' ~long_name:"outtype" ~help:"Output type" out_type;
  add options ~short_name:'o' ~long_name:"outfile" ~help:"Output file" out_file;

  let deb_group = add_group options "Debian Specific Options" in
  add options ~group:deb_group ~long_name:"deb-native-arch" ~help:"Native architecture" deb_native_arch;
  (*
  add options ~group:deb_group ~long_name:"deb-host-arch" ~help:"Host architecture" deb_host_arch;
  add options ~group:deb_group ~long_name:"deb-build-arch" ~help:"Build architecture" deb_build_arch;
  *)
  add options ~group:deb_group ~long_name:"deb-foreign-archs" ~help:"Foreign architectures" deb_foreign_arch;

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

let nr_conflicts univ =
  let open Cudf in
  Cudf.fold_packages (fun acc p ->
    let cfl = List.filter (fun x -> not(x =% p))
      (List.flatten (List.rev_map (CudfAdd.who_provides univ) p.conflicts)) in
    debug "%s: %d conflicts" (Cudf_types_pp.string_of_pkgname p.package)
      (List.length cfl);
    List.iter (fun c ->
      debug "- %s" (Cudf_types_pp.string_of_pkgname c.package)
    ) cfl;
    acc + (List.length cfl)
  ) 0 univ
;;

let output_cudf oc pr univ =
  Cudf_printer.pp_preamble oc pr;
  Printf.fprintf oc "\n";
  Cudf_printer.pp_universe oc univ
;;

let set_options = function
  |Url.Deb ->
    let host =
      if OptParse.Opt.is_set Options.deb_host_arch then
        OptParse.Opt.get Options.deb_host_arch
      else ""
    in
    let build =
      if OptParse.Opt.is_set Options.deb_build_arch then
        OptParse.Opt.get Options.deb_build_arch
      else ""
    in
    let native =
      if OptParse.Opt.is_set Options.deb_native_arch then
        OptParse.Opt.get Options.deb_native_arch
      else ""
    in
    let archs = 
      let l = OptParse.Opt.get Options.deb_foreign_arch in
      let l = if host <> "" then host::l else l in
      let l = if build <> "" then build::l else l in
      let l = if native <> "" then native::l else l in
      l
    in

    Some (
      Boilerplate.Deb {
        Debian.Debcudf.default_options with 
        Debian.Debcudf.foreign = archs;
        host = host;
        build = build;
        native = native
      }
    )
  |Url.Synthesis -> None
  |Url.Hdlist -> None
  |Url.Eclipse -> Some (Boilerplate.Eclipse Debian.Debcudf.default_options)
  |Url.Cudf -> None
  |_ -> fatal "Unkown input format"
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let input_format = Input.guess_format [posargs] in
  let options = set_options input_format in

  Boilerplate.enable_debug(OptParse.Opt.get Options.verbose);

  if OptParse.Opt.get Options.out_type = "sqlite" then
    output_to_sqlite posargs
  else
  let (preamble,universe,from_cudf,to_cudf) = Boilerplate.load_universe ~options posargs in
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
      let oc =
        if OptParse.Opt.is_set Options.out_file then 
          open_out (OptParse.Opt.get Options.out_file)
        else stdout
      in
      begin match OptParse.Opt.get Options.out_type with
      |"dot" -> 
IFDEF HASOCAMLGRAPH THEN
          DGraph.D.output_graph oc (DGraph.dependency_graph u)
ELSE
        failwith ("dot not supported: needs ocamlgraph")
END
      |"cnf" -> Printf.fprintf oc "%s" (Depsolver.output_clauses ~enc:Depsolver.Cnf u)
      |"dimacs" -> Printf.fprintf oc "%s" (Depsolver.output_clauses ~enc:Depsolver.Dimacs u)
      |"cudf" -> output_cudf oc preamble u
      |"table" ->
IFDEF HASOCAMLGRAPH THEN
        Printf.fprintf oc "%d\t%d\t%d\n"
        (Cudf.universe_size u) (DGraph.G.nb_edges (DGraph.dependency_graph u))
        (nr_conflicts u)
ELSE
        failwith ("table not supported: needs ocamlgraph")
END
      |_ -> assert false
      end ;
      close_out oc;
    ) ll
  in output [plist]
;;

main ();;
