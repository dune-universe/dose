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

open Debian
open Common
open Diagnostic

module Options = struct
  open OptParse

  let verbose = StdOpt.store_true ()
  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let checkonly = StdOpt.str_option ()
  let architecture = StdOpt.str_option ()
  let distribution = StdOpt.str_option ()
  let release = StdOpt.str_option ()
  let docuid = StdOpt.str_option ()

  let description = "Report the broken packages in a package list"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~long_name:"verbose" ~help:"Print progress and timing information" verbose;
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;

  add options ~short_name:'a' ~long_name:"architecture" ~help:"Set the default architecture" architecture;
  add options ~short_name:'r' ~long_name:"release" ~help:"Set the release name" release;
  add options ~short_name:'d' ~long_name:"distribution" ~help:"Set the distribution" distribution;
end

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs =
    let args = OptParse.OptParser.parse_argv Options.options in
    match Filename.basename(Sys.argv.(0)),args with
    |"debcheck",[] -> ["deb://-"]
    |"debcheck",l -> List.map ((^) "deb://") l
    |"eclipsecheck",l -> List.map ((^) "eclipse://") l
    |"rpmcheck",l -> List.map ((^) "synthesis://") l
    |_,_ -> args
  in
  if OptParse.Opt.get Options.verbose then Boilerplate.enable_debug () ;
  let (universe,from_cudf,_) = Boilerplate.load_universe posargs in
  Util.print_info "Solving..." ;
  let timer = Util.Timer.create "Solver" in
  Util.Timer.start timer;
  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let fmt = Format.std_formatter in
  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback = Diagnostic.print ~pp:from_cudf ~failure ~success ~explain fmt in
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
  if OptParse.Opt.is_set Options.docuid then
    Format.fprintf fmt "uid: %s\n" (OptParse.Opt.get Options.docuid);
  if OptParse.Opt.is_set Options.distribution then
    Format.fprintf fmt "distribution: %s\n" (OptParse.Opt.get Options.distribution);
  if OptParse.Opt.is_set Options.release then
    Format.fprintf fmt "release: %s\n" (OptParse.Opt.get Options.release);
  if OptParse.Opt.is_set Options.architecture then
    Format.fprintf fmt "architecture: %s\n" (OptParse.Opt.get Options.architecture)
;;

main () ;;
