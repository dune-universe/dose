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
open Diagnostic

module Options = struct
  open OptParse
  let description = "Report the broken packages in a package list"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let uuid = StdOpt.store_true ()
  let checkonly = Boilerplate.vpkglist_option ()
  let architecture = StdOpt.str_option ()
  let distribution = StdOpt.str_option ()
  let release = StdOpt.str_option ()
  let suite = StdOpt.str_option ()
  let summary = StdOpt.store_true ()
  let outfile = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;
  add options ~long_name:"summary" ~help:"Print a detailed summary" summary;

  add options ~long_name:"distrib" ~help:"Set the distribution" distribution;
  add options ~long_name:"release" ~help:"Set the release name" release;
  add options ~long_name:"suite" ~help:"Set the release name" suite;
  add options ~long_name:"arch" ~help:"Set the default architecture" architecture;
  add options ~short_name:'u' ~long_name:"uid" ~help:"Generate a unique identifier for the output document" uuid;

  add options ~short_name:'o' ~long_name:"outfile" ~help:"output file" outfile;
end

let debug fmt = Util.make_debug "Distcheck" fmt
let info fmt = Util.make_info "Distcheck" fmt
let warning fmt = Util.make_warning "Distcheck" fmt
let fatal fmt = Util.make_fatal "Distcheck" fmt

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
  let default_arch = OptParse.Opt.opt Options.architecture in
  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe ~default_arch posargs in
  let universe_size = Cudf.universe_size universe in
  let pkglist = 
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
      ) ["architecture";"source";"sourceversion"]
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
      Depsolver.listcheck ~callback universe pkglist
    else
      Depsolver.univcheck ~callback universe 
  in
  ignore(Util.Timer.stop timer ());

  if failure || success then Format.fprintf fmt "@]@.";
 
  let nb = universe_size in
  let nf = List.length pkglist in
  Format.fprintf fmt "background-packages: %d@." nb;
  Format.fprintf fmt "foreground-packages: %d@." (if nf = 0 then nb else nf);
  Format.fprintf fmt "broken-packages: %d@." i;
 
  if summary then 
    Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;
;;

main () ;;
