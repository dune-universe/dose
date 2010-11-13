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
  let checkonly = StdOpt.str_option ()
  let architecture = StdOpt.str_option ()
  let distribution = StdOpt.str_option ()
  let release = StdOpt.str_option ()
  let suite = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;

  add options ~long_name:"distrib" ~help:"Set the distribution" distribution;
  add options ~long_name:"release" ~help:"Set the release name" release;
  add options ~long_name:"suite" ~help:"Set the release name" suite;
  add options ~long_name:"arch" ~help:"Set the default architecture" architecture;
  add options ~short_name:'u' ~long_name:"uid" ~help:"Generate a unique identifier for the output document" uuid;
end

let debug fmt = Util.make_debug "Distcheck" fmt
let info fmt = Util.make_info "Distcheck" fmt
let warning fmt = Util.make_warning "Distcheck" fmt

let timer = Util.Timer.create "Solver" 

let main () =
  let posargs =
    let args = OptParse.OptParser.parse_argv Options.options in
    match Filename.basename(Sys.argv.(0)),args with
    |"debcheck",[] -> ["deb://-"]
    |"debcheck",l -> List.map ((^) "deb://") l
    |"eclipsecheck",l -> List.map ((^) "eclipse://") l
    |"rpmcheck",l -> List.map ((^) "synthesis://") l
    |_,_ -> args
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];
  let default_arch = OptParse.Opt.opt Options.architecture in
  let (universe,from_cudf,_) = Boilerplate.load_universe ~default_arch posargs in
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
  Util.Timer.start timer;
  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let fmt = Format.std_formatter in
  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback = Diagnostic.fprintf ~pp ~failure ~success ~explain fmt in
  let i =
    if OptParse.Opt.is_set Options.checkonly then 
      let pkglist = 
        List.flatten (
          List.map (Cudf.lookup_packages universe)
          (Str.split (Str.regexp ",") (OptParse.Opt.get Options.checkonly))
        )
      in
      Depsolver.listcheck ~callback universe pkglist
    else
      Depsolver.univcheck ~callback universe 
  in
  ignore(Util.Timer.stop timer ());

  if failure || success then Format.fprintf fmt "@]@.";
  Format.fprintf fmt "total-packages: %d\n" (Cudf.universe_size universe);
  Format.fprintf fmt "broken-packages: %d\n" i;
  if OptParse.Opt.get Options.uuid then
    Format.fprintf fmt "uid: %s\n" (Util.uuid ());
  if OptParse.Opt.is_set Options.distribution then
    Format.fprintf fmt "distribution: %s\n" (OptParse.Opt.get Options.distribution);
  if OptParse.Opt.is_set Options.release then
    Format.fprintf fmt "release: %s\n" (OptParse.Opt.get Options.release);
  if OptParse.Opt.is_set Options.suite then
    Format.fprintf fmt "suite: %s\n" (OptParse.Opt.get Options.suite);
  if OptParse.Opt.is_set Options.architecture then
    Format.fprintf fmt "architecture: %s\n" (OptParse.Opt.get Options.architecture)
;;

main () ;;
