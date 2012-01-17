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

open ExtLib
open Debian
open Common
open Algo

module Options = struct
  open OptParse
  let description = "Report the broken packages in a Packages list"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let summary = StdOpt.store_true ()
  let uuid = StdOpt.store_true ()
  let latest = StdOpt.store_true ()
  let checkonly = Boilerplate.vpkglist_option ()
  let architecture = StdOpt.str_option ()
  let distribution = StdOpt.str_option ()
  let release = StdOpt.str_option ()
  let suite = StdOpt.str_option ()
  let outfile = StdOpt.str_option ()
  let background = Boilerplate.incr_str_list ()
  let foreground = Boilerplate.incr_str_list ()

  open OptParser
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;
  add options ~long_name:"summary" ~help:"Print a detailed summary" summary;

  add options ~long_name:"latest" ~help:"Check only the latest version of each package" latest;
  add options ~short_name:'u' ~long_name:"uid" ~help:"Generate a unique identifier for the output document" uuid;

  add options ~long_name:"distrib" ~help:"Set the distribution" distribution;
  add options ~long_name:"release" ~help:"Set the release name" release;
  add options ~long_name:"suite" ~help:"Set the release name" suite;
  add options ~long_name:"arch" ~help:"Set the default architecture" architecture;

  add options ~long_name:"fg" 
  ~help:"Additional Packages lists that are checked and used for resolving dependencies (can be repeated)" foreground;

  add options ~long_name:"bg" 
  ~help:"Additional Packages lists that are NOT checked but used for resolving dependencies (can be repeated)" background;

  add options ~short_name:'o' ~long_name:"outfile" ~help:"output file" outfile;
end

let debug fmt = Util.make_debug __FILE__ fmt
let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt
let fatal fmt = Util.make_fatal __FILE__ fmt

let timer = Util.Timer.create "Solver" 

let main () =
  let posargs =
    let args = OptParse.OptParser.parse_argv Options.options in
    match Filename.basename(Sys.argv.(0)),args with
    |("debcheck"|"edos-debcheck"),[] -> ["deb://-"]
    |("debcheck"|"edos-debcheck"),l -> List.map ((^) "deb://") l
    |"eclipsecheck",l -> List.map ((^) "eclipse://") l
    |("rpmcheck"|"edos-rpmcheck"),l -> List.map ((^) "synth://") l
    |_,_ -> args
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck";"Depsolver_int.init_solver"] ;
  let default_arch = OptParse.Opt.opt Options.architecture in
  let fg = posargs @ (OptParse.Opt.get Options.foreground) in
  let bg = OptParse.Opt.get Options.background in
  let ((pkglist,fg_pkglist),from_cudf,to_cudf) =
    Boilerplate.load_list ~default_arch (fg,bg)
  in
  let pkglist = 
    if OptParse.Opt.get Options.latest then
      let h = Hashtbl.create (List.length pkglist) in
      List.iter (fun p ->
        try
          let q = Hashtbl.find h p.Cudf.package in
          if (CudfAdd.compare p q) > 0 then
            Hashtbl.replace h p.Cudf.package p
          else ()
        with Not_found -> Hashtbl.add h p.Cudf.package p
      ) pkglist;
      Hashtbl.fold (fun _ v acc -> v::acc) h []
    else
      pkglist
  in
  let universe = Cudf.load_universe pkglist in
  let universe_size = Cudf.universe_size universe in
  let checklist = 
    if OptParse.Opt.is_set Options.checkonly then 
        List.flatten (
          List.map (function 
            |(p,None) -> Cudf.lookup_packages universe p
            |(p,Some(c,v)) ->
                let filter = Some(c,snd(to_cudf (p,v))) in
                Cudf.lookup_packages ~filter universe p
          ) (OptParse.Opt.get Options.checkonly)
        )
    else []
  in
  let pp pkg =
    let (p,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in 
    let l = 
      List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["architecture";"source";"sourcenumber"]
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

  if OptParse.Opt.get Options.uuid then
    Format.fprintf fmt "uid: %s@." (Util.uuid ());
  if OptParse.Opt.is_set Options.distribution then
    Format.fprintf fmt "distribution: %s@." (OptParse.Opt.get Options.distribution);
  if OptParse.Opt.is_set Options.release then
    Format.fprintf fmt "release: %s@." (OptParse.Opt.get Options.release);
  if OptParse.Opt.is_set Options.suite then
    Format.fprintf fmt "suite: %s@." (OptParse.Opt.get Options.suite);
  if OptParse.Opt.is_set Options.architecture then
    Format.fprintf fmt "architecture: %s@." (OptParse.Opt.get Options.architecture);

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d =
    if summary then Diagnostic.collect results d ;
    Diagnostic.fprintf ~pp ~failure ~success ~explain fmt d
  in
  Util.Timer.start timer;
  let i =
    if OptParse.Opt.is_set Options.checkonly then 
      Depsolver.listcheck ~callback universe checklist
    else
      Depsolver.univcheck ~callback universe 
  in
  ignore(Util.Timer.stop timer ());

  if failure || success then Format.fprintf fmt "@]@.";
 
  let nb = universe_size in
  let nf = List.length checklist in
  Format.fprintf fmt "background-packages: %d@." nb;
  Format.fprintf fmt "foreground-packages: %d@." (if nf = 0 then nb else nf);
  Format.fprintf fmt "broken-packages: %d@." i;
 
  if summary then 
    Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;
;;

main () ;;
