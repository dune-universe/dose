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
  module PGraph = Defaultgraphs.PackageGraph
END

module Options = struct
  open OptParse
  let description = "Ceve - integrated metadata parser and transformer"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  exception Format
  let otypes = ["cnf";"dimacs";"mzn";"cudf";"dot";"gml";"grml";"table"] 
  let out_option ?default ?(metavar = Printf.sprintf "<%s>" (String.concat "|" otypes)) () =
    let corce s = if List.mem s otypes then s else raise Format in
    let error _ s = Printf.sprintf "%s format not supported" s in
    Opt.value_option metavar default corce error

  let gtypes = ["syn";"pkg";"conj";"strdeps";"strcnf";"dom"]
  let grp_option ?default ?(metavar = Printf.sprintf "<%s>" (String.concat "|" gtypes)) () =
    let corce s = if List.mem s gtypes then s else raise Format in
    let error _ s = Printf.sprintf "%s format not supported" s in
    Opt.value_option metavar default corce error

  let src = Boilerplate.vpkglist_option ()
  let dst = Boilerplate.vpkg_option ()
  let cone = Boilerplate.vpkglist_option ()
  let reverse_cone = Boilerplate.vpkglist_option ()
  let cone_maxdepth = StdOpt.int_option ()
  let out_type = out_option ~default:"cnf" ()
  let grp_type = grp_option ~default:"syn" ()
  let request = Boilerplate.incr_str_list ()

  open OptParser
  add options ~short_name:'c' ~long_name:"cone" ~help:"dependency cone" cone;
  add options ~short_name:'r' ~long_name:"rcone" ~help:"reverse dependency cone" reverse_cone;
  add options                 ~long_name:"depth" ~help:"max depth - in conjunction with cone" cone_maxdepth;
  add options ~short_name:'G' ~help:"Graph output type. Default syn" grp_type;
  add options ~short_name:'T' ~help:"Output type format. Default cnf" out_type;
  add options                 ~long_name:"request" ~help:"Installation Request (can be repeated)" request;

  include Boilerplate.InputOptions
  Boilerplate.InputOptions.add_options ~default:["outfile";"latest";"trim"] options ;;

  include Boilerplate.DistribOptions;;
  Boilerplate.DistribOptions.add_options options ;;

end;;

(* -------------------------------- *)

let loadl to_cudf l =
  List.flatten (
    List.map (fun ((name,aop),sel) ->
      let encname =
        let n = match aop with Some a -> name^":"^a | None -> name in
        CudfAdd.encode n
      in
      match CudfAdd.cudfop sel with
      |None -> [(encname, None)]
      |Some(op,v) ->
          [(encname,Some(op,snd(to_cudf (encname,v))))]
    ) l
  )
;;

let parse_request to_cudf l =
  let open Debian in
  let parse acc s =
    if String.starts_with s "install: " then
      let rs = String.strip (snd (String.split s " ")) in
      let f = Packages.lexbuf_wrapper Packages_parser.vpkglist_top in
      { acc with Cudf.install = loadl to_cudf (f (Format822.dummy_loc,rs)) }
    else if String.starts_with s "remove: " then
      let rs = String.strip (snd (String.split s " ")) in
      let f = Packages.lexbuf_wrapper Packages_parser.vpkglist_top in
      { acc with Cudf.remove = loadl to_cudf (f (Format822.dummy_loc,rs)) } 
    else if String.starts_with s "upgrade: " then
      let rs = String.strip (snd (String.split s " ")) in
      let f = Packages.lexbuf_wrapper Packages_parser.vpkglist_top in
      { acc with Cudf.upgrade = loadl to_cudf (f (Format822.dummy_loc,rs)) }
    else acc
  in
  List.fold_left parse Cudf.default_request l
;;

let output_to_sqlite args =
IFDEF HASDB THEN
  begin
    let pl = List.unique (List.flatten (List.map (function u ->
      match Input.parse_uri u with
      |("deb", (_, _, _, _, f), _) -> Debian.Packages.input_raw [f]
      |_ -> failwith "Other file formats than Debian are not yet supported for SQLite insertion"
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

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let (input_type,implicit) =
    if OptParse.Opt.is_set Options.inputtype then 
      (Url.scheme_of_string (OptParse.Opt.get Options.inputtype),true)
    else
      (Input.guess_format [posargs],false)
  in
  let options = Options.set_options input_type in

  Boilerplate.enable_debug(OptParse.Opt.get Options.verbose);
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let global_constraints = not(OptParse.Opt.get Options.deb_ignore_essential) in

  if OptParse.Opt.get Options.out_type = "sqlite" then
    output_to_sqlite posargs
  else
  let (fg,bg) = Options.parse_cmdline (input_type,implicit) posargs in
  let (preamble,pkgll,from_cudf,to_cudf) = Boilerplate.load_list ~options [fg;bg] in
  let (fg_pkglist, bg_pkglist) = match pkgll with [fg;bg] -> (fg,bg) | _ -> assert false in
  let universe =
    let s = CudfAdd.to_set (fg_pkglist @ bg_pkglist) in
    let u = 
      if OptParse.Opt.get Options.latest then
        Cudf.load_universe (CudfAdd.latest (CudfAdd.Cudf_set.elements s))
      else 
        Cudf.load_universe (CudfAdd.Cudf_set.elements s)
    in
    if OptParse.Opt.get Options.trim then Depsolver.trim ~global_constraints u else u
  in

  let request = parse_request to_cudf (OptParse.Opt.get Options.request) in
  let get_cudfpkglist ((n,a),c) = 
    let (name,filter) = Debian.Debutil.debvpkg to_cudf ((n,a),c) in
    try Cudf.lookup_packages ~filter universe name
    with ExtList.List.Empty_list -> fatal "package %s not found" n
  in
  let get_cudfpkg ((n,a),c) = List.hd (get_cudfpkglist ((n,a),c)) in

  let pkg_src () = List.map get_cudfpkg (OptParse.Opt.get Options.src) in
  let pkg_dst () =
    (* all packages q in R s.t. q is in the dependency closure of p *)
    let (p,v) = OptParse.Opt.get Options.dst in
    let pid = get_cudfpkg (p,v) in
    List.filter_map (fun pkg ->
      if List.mem pid (Depsolver.dependency_closure universe [pkg]) then
        Some(pkg)
      else None
    ) (Cudf.get_packages universe) 
  in
  let pkg_cone () =
    List.unique (List.fold_left (fun acc (p,c) ->
      let l = get_cudfpkglist (p,c) in
      if OptParse.Opt.is_set Options.cone_maxdepth then
        let md = OptParse.Opt.get Options.cone_maxdepth in
        (Depsolver.dependency_closure ~maxdepth:md universe l) @ acc
      else
        (Depsolver.dependency_closure universe l) @ acc
    ) [] (OptParse.Opt.get Options.cone))
  in
  let pkg_reverse_cone () =
    List.unique (List.fold_left (fun acc (p,c) ->
      let l = get_cudfpkglist (p,c) in
      if OptParse.Opt.is_set Options.cone_maxdepth then
        let md = OptParse.Opt.get Options.cone_maxdepth in
        (Depsolver.reverse_dependency_closure ~maxdepth:md universe l) @ acc
      else
        (Depsolver.reverse_dependency_closure universe l) @ acc
    ) [] (OptParse.Opt.get Options.reverse_cone))
  in

  let pkg_src_list = ref [] in
  let pkg_dst_list = ref [] in
  let plist =
    if OptParse.Opt.is_set Options.src && OptParse.Opt.is_set Options.dst then begin
      let (p,v) = OptParse.Opt.get Options.dst in
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
      let doc = (preamble,u,request) in
      let oc =
        if OptParse.Opt.is_set Options.outfile then 
          open_out (OptParse.Opt.get Options.outfile)
        else stdout
      in
      begin match OptParse.Opt.get Options.out_type with
      |"cnf" -> Printf.fprintf oc "%s" (Depsolver.output_clauses ~global_constraints ~enc:Depsolver.Cnf u)
      |"mzn" -> Printf.fprintf oc "%s" (Depsolver.output_minizinc ~global_constraints doc)
      |"dimacs" -> Printf.fprintf oc "%s" (Depsolver.output_clauses ~global_constraints ~enc:Depsolver.Dimacs u)
      |"cudf" -> Cudf_printer.pp_cudf oc doc
      |"table" ->
IFDEF HASOCAMLGRAPH THEN
        Printf.fprintf oc "%d\t%d\t%d\n"
        (Cudf.universe_size u) (DGraph.G.nb_edges (DGraph.dependency_graph u))
        (nr_conflicts u)
ELSE
        failwith (Printf.sprintf "format %s not supported: needs ocamlgraph" t)
END

      |("dot" | "gml" | "grml") as t -> 
IFDEF HASOCAMLGRAPH THEN
        let fmt = Format.formatter_of_out_channel oc in
        begin match OptParse.Opt.get Options.grp_type with
          |"syn" ->
            let g = DGraph.dependency_graph u in
            if t = "dot" then DGraph.DotPrinter.print fmt g
            else if t = "gml" then DGraph.GmlPrinter.print fmt g
            else if t = "grml" then DGraph.GraphmlPrinter.print fmt g
            else assert false
          |("pkg" | "strdeps" | "conj"| "dom") as gt ->
            let g =
              if gt = "pkg" then PGraph.dependency_graph u
              else if gt = "strdeps" then Strongdeps.strongdeps_univ u
              else if gt = "conj" then Strongdeps.conjdeps_univ u
              else if gt = "dom" then
                let g = Strongdeps.strongdeps_univ ~transitive:true universe in
                Dominators.dominators_tarjan g
              else assert false
            in
            if t = "dot" then
              PGraph.DotPrinter.print fmt g
            else if t = "gml" then
              PGraph.GmlPrinter.print fmt g
            else if t = "grml" then
              PGraph.GraphmlPrinter.print fmt g
            else assert false
          |s -> failwith (Printf.sprintf "type %s not supported" s)
        end
ELSE
        failwith (Printf.sprintf "format %s not supported: needs ocamlgraph" t)
END
      |_ -> assert false
      end ;
      close_out oc;
    ) ll
  in output [plist]
;;

Boilerplate.if_application ~alternatives:["dose-ceve";"ceve"] __FILE__ (fun () -> main (); 0) ;;
