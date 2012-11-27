(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2012 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

open ExtLib
open Common
open Algo
open Debian

module Options = struct
  open OptParse
  let description = ("Check for Debian Package Coinstallability."^
    "Return the list of binary package. If --src <Sources> is specified"^
    "then we return the list of corresponding source packages")
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let minimal = StdOpt.store_true ()
  let summary = StdOpt.store_true ()
  let latest = StdOpt.store_true ()
  let checkonly = Boilerplate.vpkglist_option ()

  let prod = StdOpt.store_true ()

  let outfile = StdOpt.str_option ()
  let background = Boilerplate.incr_str_list ()
  let foreground = Boilerplate.incr_str_list ()
  let sources = StdOpt.str_option ()

  open OptParser

  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'m' ~long_name:"explain-minimal" ~help:"" minimal;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;

  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;

  add options ~long_name:"latest" ~help:"Check only the latest version of each package" latest;

  add options ~long_name:"prod" ~help:"Check the product coinstallability" prod;

  add options ~long_name:"fg" 
  ~help:"Additional Packages lists that are checked and used for resolving dependencies (can be repeated)" foreground;

  add options ~long_name:"bg" 
  ~help:"Additional Packages lists that are NOT checked but used for resolving dependencies (can be repeated)" background;

  add options ~long_name:"src" ~help:"Associate Sources file" sources;

  add options ~short_name:'o' ~long_name:"outfile" ~help:"Set the output file" outfile;

  include Boilerplate.MakeDistribOptions(struct let options = options end);;

end

include Util.Logging(struct let label = __FILE__ end) ;;

let timer = Util.Timer.create "Solver" 

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck";"Depsolver_int.init_solver"] ;
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let options = Options.set_deb_options () in

  let fg = OptParse.Opt.get Options.foreground in
  let bg = OptParse.Opt.get Options.background in
  let fg =
    let pos =
      if List.length (posargs@fg@bg) = 0 then 
        ["-"] 
      else 
        posargs
    in
    pos@fg
  in

  let cudftodeb_table = Hashtbl.create 30000 in
  let cudftosrc_table = Hashtbl.create 30000 in

  let deb_load_list options ?(status=[]) sources urilist =
    let native = options.Debian.Debcudf.native in
    let archs =
      if native <> "" then
        native :: options.Debian.Debcudf.foreign
      else []
    in
    let dll =
      List.map (fun filelist ->
        Debian.Packages.input_raw ~archs filelist
      ) urilist
    in
    let pkglist = List.flatten dll in
    let pkglist = if status = [] then pkglist else Debian.Packages.merge status pkglist in
    let srclist = 
      if not(Option.is_none sources) then
        let l = Sources.input_raw ~archs [Option.get sources] in
        Sources.sources2packages ~noindep:true ~profiles:false native l
      else []
    in
    let tables = Debian.Debcudf.init_tables (srclist@pkglist) in
    let from_cudf (p,i) = (p,Debian.Debcudf.get_real_version tables (p,i)) in
    let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
    let srcl =
      let dl = List.map (Debian.Debcudf.tocudf tables ~options) srclist in
      List.iter2 (fun cudfpkg -> fun srcpkg ->
        let id = (cudfpkg.Cudf.package,cudfpkg.Cudf.version) in
        Hashtbl.add cudftosrc_table id srcpkg
      ) dl srclist;
      dl
    in

    let cll =
      List.map (fun l ->
        List.map (fun debpkg ->
          let cudfpkg = Debian.Debcudf.tocudf tables ~options debpkg in
          let id = (cudfpkg.Cudf.package,cudfpkg.Cudf.version) in
          Hashtbl.add cudftodeb_table id debpkg;
          cudfpkg
        ) (Debian.Packages.merge status l)
      ) dll
    in
    let preamble = Debian.Debcudf.preamble in
    (preamble,cll,srcl,from_cudf,to_cudf)
  in

  let sources = OptParse.Opt.opt Options.sources in
  let (preamble,pkgll,srclist,from_cudf,to_cudf) = deb_load_list options sources [fg;bg] in

  let (fg_pkglist, bg_pkglist) = match pkgll with [fg;bg] -> (fg,bg) | _ -> assert false in

  let fg_pkglist = 
    if OptParse.Opt.get Options.latest then CudfAdd.latest fg_pkglist
    else fg_pkglist
  in
  let universe = 
    let s = CudfAdd.to_set (srclist @ fg_pkglist @ bg_pkglist) in
    Cudf.load_universe (CudfAdd.Cudf_set.elements s) 
  in
  let universe_size = Cudf.universe_size universe in

  let checklist = 
    if OptParse.Opt.is_set Options.checkonly then begin
      info "--checkonly specified, consider all packages as background packages";
      List.flatten (
        List.map (fun ((n,a),c) ->
          let (name,filter) = Debian.Debutil.debvpkg to_cudf ((n,a),c) in
          Cudf.lookup_packages ~filter universe name
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else fg_pkglist
  in

  let pp = CudfAdd.pp from_cudf in

  info "Solving..." ;
  let failure = OptParse.Opt.get Options.failures in
  let explain = OptParse.Opt.get Options.explain in
  let minimal = OptParse.Opt.get Options.minimal in
  let fmt =
    if OptParse.Opt.is_set Options.outfile then
      let oc = open_out (OptParse.Opt.get Options.outfile) in
      Format.formatter_of_out_channel oc
    else
      Format.std_formatter
  in

  if failure then Format.fprintf fmt "@[<v 1>report:@,";

  let global_constraints = not(OptParse.Opt.get Options.deb_ignore_essential) in

  Util.Timer.start timer;
  let result = Depsolver.edos_coinstall ~global_constraints universe checklist in
  ignore(Util.Timer.stop timer ());

  if Diagnostic.is_solution result then
    let is = Diagnostic.get_installationset result in
    if Option.is_none sources then 
      List.iter (fun cudfpkg ->
        try
          let id = (cudfpkg.Cudf.package,cudfpkg.Cudf.version) in
          let debpkg = Hashtbl.find cudftodeb_table id in
          Printf.printf "%a\n" Debian.Printer.pp_package debpkg
        with Not_found -> assert false
      ) is
    else
      let l = 
        CudfAdd.unique (
          List.map (fun binpkg ->
            let cudfpkg = Sources.get_src_package universe binpkg in
            let id = (cudfpkg.Cudf.package,cudfpkg.Cudf.version) in
            Hashtbl.find cudftosrc_table id
          ) is
        )
      in
      List.iter (Printf.printf "%a\n" Debian.Printer.pp_package) l
  else
    Diagnostic.fprintf ~pp ~minimal ~failure ~explain fmt result;
  
  if failure then Format.fprintf fmt "@]@.";
  
  let fn = List.length fg_pkglist in
  let bn = List.length bg_pkglist in
  
  let nb,nf = 
    let cl = List.length checklist in
    if cl != 0 then ((fn + bn) - cl,cl) else (bn,fn)
  in
  
  if nb > 0 && failure then begin
    Format.fprintf fmt "background-packages: %d@." nb;
    Format.fprintf fmt "foreground-packages: %d@." nf
  end;

  if failure then begin
    Format.fprintf fmt "total-packages: %d@." universe_size;
  (*  Format.fprintf fmt "broken-packages: %d@." nbp; *)
  end;
  Boilerplate.exit(0)
;;

Boilerplate.if_application
~alternatives:["dose-debcoinstall";"deb-coinstall"] __FILE__ main ;;

