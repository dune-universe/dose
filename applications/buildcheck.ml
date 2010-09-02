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
module Src = Debian.Sources
module Deb = Debian.Packages

module Options = struct
  open OptParse

  let verbose = StdOpt.incr_option ()
  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let xml = StdOpt.str_option ()
  let architecture = StdOpt.str_option ()

  let showall () = (Opt.get successes) && (Opt.get failures)
  let onlyfail () = (Opt.get failures) && not (Opt.get successes)
  let onlysucc () = (Opt.get successes) && not (Opt.get failures)

  let description = "Report the broken packages in a package list"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~help:"Print information (can be repeated)" verbose;
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;
  add options ~short_name:'a' ~long_name:"architecture" ~help:"" architecture;
  add options ~long_name:"xml" ~help:"Output results in XML format" xml;
end

let debug fmt = Util.make_debug "Buildcheck" fmt
let info fmt = Util.make_info "Buildcheck" fmt
let warning fmt = Util.make_warning "Buildcheck" fmt

open Diagnostic

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);

  if not(OptParse.Opt.is_set Options.architecture) then begin
    Printf.eprintf "--architecture must be specified\n";
    exit 1;
  end ;

  let pkglist = Deb.input_raw [List.hd posargs] in
  let srclist = 
    let l = Src.input_raw (List.tl posargs) in
    Src.sources2packages (OptParse.Opt.get Options.architecture) l 
  in
  let tables = Debcudf.init_tables (srclist @ pkglist) in
 
  let sl = List.map (fun pkg -> Debcudf.tocudf tables pkg) srclist in
  let l = List.fold_left (fun acc pkg -> (Debcudf.tocudf tables pkg)::acc) sl pkglist in
  let universe = Cudf.load_universe l in

  let from_cudf pkg =
    let (p,i) = (pkg.Cudf.package,pkg.Cudf.version) in
    let v = Debian.Debcudf.get_real_version tables (p,i) in
    (p,v)
  in

  info "Solving..." ;
  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let callback = Diagnostic.printf ~pp:from_cudf ~failure ~success ~explain in
  let i = Depsolver.listcheck ~callback universe sl in
  Printf.eprintf "Broken Packages: %d\n" i
;;

main () ;;
