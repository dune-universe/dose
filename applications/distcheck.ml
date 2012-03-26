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
open Debian
open Common
open Algo

module Options = struct
  open OptParse
  let description = "Report the broken packages in a Packages list"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let inputtype = StdOpt.str_option ()
  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let summary = StdOpt.store_true ()
  let latest = StdOpt.store_true ()
  let checkonly = Boilerplate.vpkglist_option ()
  let coinst = Boilerplate.vpkglist_option ()

  let outfile = StdOpt.str_option ()
  let background = Boilerplate.incr_str_list ()
  let foreground = Boilerplate.incr_str_list ()

  let deb_foreign_arch = Boilerplate.str_list_option ()
  let deb_native_arch = StdOpt.str_option ()
  let deb_host_arch = StdOpt.str_option ()
  let deb_build_arch = StdOpt.str_option ()
  let deb_ignore_essential = StdOpt.store_true ()

  open OptParser

  add options ~short_name:'t' ~help:"input type format" inputtype;

  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;
  add options ~long_name:"summary" ~help:"Print a detailed summary" summary;

  add options ~long_name:"latest" ~help:"Check only the latest version of each package" latest;

  add options ~long_name:"coinst" ~help:"Check if these packages are coinstallable" coinst;

  add options ~long_name:"fg" 
  ~help:"Additional Packages lists that are checked and used for resolving dependencies (can be repeated)" foreground;

  add options ~long_name:"bg" 
  ~help:"Additional Packages lists that are NOT checked but used for resolving dependencies (can be repeated)" background;

  add options ~short_name:'o' ~long_name:"outfile" ~help:"output file" outfile;

  let deb_group = add_group options "Debian Specific Options" in
  add options ~group:deb_group ~long_name:"deb-native-arch" ~help:"Native architecture" deb_native_arch;
  (*
  add options ~group:deb_group ~long_name:"deb-host-arch" ~help:"Host architecture" deb_host_arch;
  add options ~group:deb_group ~long_name:"deb-build-arch" ~help:"Build architecture" deb_build_arch;
  *)
  add options ~group:deb_group ~long_name:"deb-foreign-archs" ~help:"Foreign architectures" deb_foreign_arch;
  add options ~group:deb_group ~long_name:"deb-ignore-essential" ~help:"Ignore Essential Packages" deb_ignore_essential;

(*  let rpm_group = add_group options "Rpm Specific Options" in
    let eclipse_group = add_group options "Eclipse Specific Options" in
*)
end

include Util.Logging(struct let label = __FILE__ end) ;;

let timer = Util.Timer.create "Solver" 

(* implicit prefix of resources derived from name of executable *)
(* (input_format * add_format ?) *)
let guess_format t l =
  match Filename.basename(Sys.argv.(0)) with
  |"debcheck"|"dose-debcheck" -> (Url.Deb, true)
  |"eclipsecheck"|"dose-eclipsecheck" -> (Url.Eclipse, true)
  |"rpmcheck"|"dose-rpmcheck" -> (Url.Synthesis,true)
  |_ when OptParse.Opt.is_set t -> 
      (Url.scheme_of_string (OptParse.Opt.get t),true)
  |_ -> (Input.guess_format [l], false)
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
        native = native;
        ignore_essential = OptParse.Opt.get Options.deb_ignore_essential
      }
    )
  |Url.Synthesis -> None
  |Url.Hdlist -> None
  |(Url.Pgsql|Url.Sqlite) -> None
  |Url.Eclipse -> Some (Boilerplate.Eclipse Debian.Debcudf.default_options)
  |Url.Cudf -> None
  |Url.Cws -> Some (Boilerplate.Cws Debian.Debcudf.default_options)
  |_ -> fatal "Unknown Url format"
;;

let add_format t = List.map (fun s -> (Url.scheme_to_string t)^"://"^s)

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let inputlist = posargs@(OptParse.Opt.get Options.foreground) in
  let (input_format,implicit_format) = guess_format Options.inputtype inputlist in

  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck";"Depsolver_int.init_solver"] ;
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let options = set_options input_format in

  let fg = OptParse.Opt.get Options.foreground in
  let bg = OptParse.Opt.get Options.background in
  let fg =
    let pos =
      if List.length (posargs@fg@bg) = 0 && implicit_format then 
        ["-"] 
      else 
        posargs
    in
    if implicit_format then add_format input_format (pos@fg) else (pos@fg)
  in
  let bg = if implicit_format then add_format input_format bg else bg in

  let (preamble,pkgll,from_cudf,to_cudf) = Boilerplate.load_list ~options [fg;bg] in
  let (fg_pkglist, bg_pkglist) = match pkgll with [fg;bg] -> (fg,bg) | _ -> assert false in
  let fg_pkglist = 
    if OptParse.Opt.get Options.latest then
      let h = Hashtbl.create (List.length fg_pkglist) in
      List.iter (fun p ->
        try
          let q = Hashtbl.find h p.Cudf.package in
          if (CudfAdd.compare p q) > 0 then
            Hashtbl.replace h p.Cudf.package p
          else ()
        with Not_found -> Hashtbl.add h p.Cudf.package p
      ) fg_pkglist;
      Hashtbl.fold (fun _ v acc -> v::acc) h []
    else
      fg_pkglist
  in
  let universe = 
    let s = CudfAdd.to_set (fg_pkglist @ bg_pkglist) in
    Cudf.load_universe (CudfAdd.Cudf_set.elements s) 
  in
  let universe_size = Cudf.universe_size universe in

  if OptParse.Opt.is_set Options.checkonly && 
    OptParse.Opt.is_set Options.coinst then
      fatal "--checkonly and --coinst cannot be speficied together";

  let checklist = 
    if OptParse.Opt.is_set Options.checkonly then begin
      info "--checkonly specified, consider all packages as background packages";
      List.flatten (
        List.map (function 
          |(p,None) -> Cudf.lookup_packages universe p
          |(p,Some(c,v)) ->
              let filter = Some(c,snd(to_cudf (p,v))) in
              Cudf.lookup_packages ~filter universe p
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else []
  in

  let coinstlist = 
    if OptParse.Opt.is_set Options.coinst then begin
      info "--coinst specified, consider all packages as background packages";
      List.map (function 
        |(p,None) -> Cudf.lookup_packages universe p
        |(p,Some(c,v)) ->
            let filter = Some(c,snd(to_cudf (p,v))) in
            Cudf.lookup_packages ~filter universe p
      ) (OptParse.Opt.get Options.coinst)
    end else []
  in

  let pp pkg =
    let (p,v) = (*CudfAdd.decode pkg.Cudf.package,CudfAdd.string_of_version pkg*) ( from_cudf
    (pkg.Cudf.package,pkg.Cudf.version) ) in 
    let l = 
      List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["architecture";"source";"sourcenumber";"essential"]
    in (p,v,l)
  in
  info "Solving..." ;
  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let summary = OptParse.Opt.get Options.summary in
  let fmt =
    if OptParse.Opt.is_set Options.outfile then
      let oc = open_out (OptParse.Opt.get Options.outfile) in
      Format.formatter_of_out_channel oc
    else
      Format.std_formatter
  in
  let results = Diagnostic.default_result universe_size in

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d =
    if summary then Diagnostic.collect results d ;
    Diagnostic.fprintf ~pp ~failure ~success ~explain fmt d
  in
  Util.Timer.start timer;

  let number_broken =
    if OptParse.Opt.is_set Options.coinst then 
      let rl = Depsolver.edos_coinstall_prod universe coinstlist in
      let number_broken_tuples =
	List.length (List.filter (fun r -> not (Diagnostic.is_solution r)) rl) 
      and number_checks = List.length rl
      in begin
	ignore(Util.Timer.stop timer ());
	List.iter callback rl;
	if failure || success then Format.fprintf fmt "@]@.";
	Format.fprintf fmt "total-packages: %d@." universe_size;
	Format.fprintf fmt "total-tuples: %d@." number_checks;
	Format.fprintf fmt "broken-tuples: %d@." number_broken_tuples;
	number_broken_tuples
      end
    else begin 
      let number_broken_packages =
	if OptParse.Opt.is_set Options.checkonly then 
	  Depsolver.listcheck ~callback universe checklist
	else begin
	  if bg_pkglist = [] then
            Depsolver.univcheck ~callback universe 
	  else
            Depsolver.listcheck ~callback universe fg_pkglist
	end
      in
      ignore(Util.Timer.stop timer ());
      
      if failure || success then Format.fprintf fmt "@]@.";
      
      let fn = List.length fg_pkglist in
      let bn = List.length bg_pkglist in
      
      let nb,nf = 
	let cl = List.length checklist in
	if cl != 0 then ((fn + bn) - cl,cl) else (bn,fn)
      in
      
      Format.fprintf fmt "background-packages: %d@." nb;
      Format.fprintf fmt "foreground-packages: %d@." nf;
      Format.fprintf fmt "total-packages: %d@." universe_size;
      Format.fprintf fmt "broken-packages: %d@." number_broken_packages;
      if summary then 
	Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;
      number_broken_packages
    end
      
  in
  (* if at least one broken package then we set the exit code = 1 *)
  if number_broken > 0 then exit(1);
;;

main () ;;
