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

module Options = struct
  open OptParse
  let description = "Report the broken packages in a Packages list"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let inputtype = StdOpt.str_option ()
  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let minimal = StdOpt.store_true ()
  let summary = StdOpt.store_true ()
  let latest = StdOpt.store_true ()
  let checkonly = Boilerplate.vpkglist_option ()
  let coinst = Boilerplate.vpkglist_option ()

  let outfile = StdOpt.str_option ()
  let background = Boilerplate.incr_str_list ()
  let foreground = Boilerplate.incr_str_list ()

  open OptParser

  add options ~short_name:'t' ~help:"input type format" inputtype;

  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'m' ~long_name:"explain-minimal" ~help:"" minimal;
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

  include Boilerplate.MakeDistribOptions(struct let options = options end);;

end

include Util.Logging(struct let label = __FILE__ end) ;;

let timer = Util.Timer.create "Solver" 

(* implicit prefix of resources derived from name of executable *)
(* (input_format * add_format ?) *)
let guess_format t l =
  match Filename.basename(Sys.argv.(0)) with
  |"debcheck"|"dose-debcheck" -> (`Deb, true)
  |"eclipsecheck"|"dose-eclipsecheck" -> (`Eclipse, true)
  |"rpmcheck"|"dose-rpmcheck" -> (`Synthesis,true)
  |_ when OptParse.Opt.is_set t -> 
      (Url.scheme_of_string (OptParse.Opt.get t),true)
  |_ -> (Input.guess_format [l], false)
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

  let options = Options.set_options input_format in

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
    if OptParse.Opt.get Options.latest then CudfAdd.latest fg_pkglist
    else fg_pkglist
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
        List.map (fun ((n,a),c) ->
          let (name,filter) = Debian.Debutil.debvpkg to_cudf ((n,a),c) in
          Cudf.lookup_packages ~filter universe name
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else []
  in

  let coinstlist = 
    if OptParse.Opt.is_set Options.coinst then begin
      info "--coinst specified, consider all packages as background packages";
      List.map (fun ((n,a),c) ->
        let (name,filter) = Debian.Debutil.debvpkg to_cudf ((n,a),c) in
        Cudf.lookup_packages ~filter universe name
      ) (OptParse.Opt.get Options.coinst)
    end else []
  in
  let pp = CudfAdd.pp from_cudf in

  info "Solving..." ;
  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let minimal = OptParse.Opt.get Options.minimal in
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
    let pp =
      if input_format = `Cudf then 
        fun pkg -> pp ~decode:(fun x -> x) pkg 
      else fun pkg -> pp pkg
    in
    Diagnostic.fprintf ~pp ~failure ~success ~explain ~minimal fmt d
  in
  Util.Timer.start timer;

  let number_broken =
    if OptParse.Opt.is_set Options.coinst then 
      let rl = Depsolver.edos_coinstall_prod universe coinstlist in
      let nbt = List.length (List.filter (fun r -> not (Diagnostic.is_solution r)) rl) in
      let number_checks = List.length rl in 
      ignore(Util.Timer.stop timer ());
      List.iter callback rl;
      if failure || success then Format.fprintf fmt "@]@.";
      Format.fprintf fmt "total-packages: %d@." universe_size;
      Format.fprintf fmt "total-tuples: %d@." number_checks;
      Format.fprintf fmt "broken-tuples: %d@." nbt;
      nbt
    else begin 
      let global_constraints = not(OptParse.Opt.get Options.deb_ignore_essential) in
      let nbp =
	if OptParse.Opt.is_set Options.checkonly then 
	  Depsolver.listcheck ~global_constraints ~callback universe checklist
	else
	  if bg_pkglist = [] then
            Depsolver.univcheck ~global_constraints ~callback universe 
	  else
            Depsolver.listcheck ~global_constraints ~callback universe fg_pkglist
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
      (*
      Format.fprintf fmt "broken-percent: %0.2f%%@." 
       ( (float_of_int nbp) /.  (float_of_int universe_size) *. 100. ) ;
      *)
      Format.fprintf fmt "broken-packages: %d@." nbp;
      if summary then 
	Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;
      nbp
    end
      
  in
  (* if at least one broken package then we set the exit code = 1 *)
  if number_broken > 0 then exit(1);
;;

main () ;;
