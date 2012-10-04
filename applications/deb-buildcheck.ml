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
module Src = Debian.Sources
module Deb = Debian.Packages
module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse
  let description =
    "Report the broken packages in a debian source list. \
     You must provide a (list of) Debian Packages file(s) and \
     a Debian Sources file in this order"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let checkonly = Boilerplate.vpkglist_option ()
  let latest = StdOpt.store_true ()
  let summary = StdOpt.store_true ()
  let dump = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;
  add options ~long_name:"latest" ~help:"Check only the latest version of each package" latest;
  add options ~long_name:"summary" ~help:"Print a detailed summary" summary;

  add options ~long_name:"dump" ~help:"dump the cudf file" dump;

  include Boilerplate.MakeDistribOptions(struct let options = options end);;

end

include Util.Logging(struct let label = __FILE__ end) ;;

let timer = Util.Timer.create "Solver"

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];

  let options = Options.set_deb_options () in
  let target = options.Debian.Debcudf.target in
  let builddepsarchs = Options.get_deb_buildarchs options in

  let pkglist, srclist =
    match posargs with
    |[] | [_] -> fatal 
      "You must provide a list of Debian Packages files and \
       a Debian Sources file"
    |l -> 
        begin match List.rev l with
        |h::t ->
          let srclist = Boilerplate.deb_load_source target builddepsarchs h in
          let pkglist = Deb.input_raw t in
          (pkglist,srclist)
        |_ -> fatal "An impossible situation occurred ?!#"
        end
  in
  let tables = Debcudf.init_tables (srclist @ pkglist) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let from_cudf (p,v) = (p,Debian.Debcudf.get_real_version tables (p,v)) in
  let pp = CudfAdd.pp from_cudf in 

  (* XXX here latest could be a bit faster if done at the same time of the cudf
     conversion *)
  let sl = 
    let l = List.map (fun pkg -> Debcudf.tocudf ~options tables pkg) srclist in
    if OptParse.Opt.get Options.latest then
      CudfAdd.latest l
    else
      l
  in
  let bl = List.fold_left (fun acc pkg -> (Debcudf.tocudf ~options tables pkg)::acc) sl pkglist in

  let universe = Cudf.load_universe bl in
  let universe_size = Cudf.universe_size universe in

  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let summary = OptParse.Opt.get Options.summary in

  let checklist =
    if OptParse.Opt.is_set Options.checkonly then begin
      List.flatten (
        List.map (fun ((n,a),c) ->
          let (name,filter) = Debian.Debutil.debvpkg to_cudf (("src:"^n,a),c) in
          Cudf.lookup_packages ~filter universe name
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else sl
  in

  let fmt = Format.std_formatter in

  let results = Diagnostic.default_result universe_size in

  Format.fprintf fmt "native-architecture: %s@." (OptParse.Opt.get Options.deb_native_arch);

  if OptParse.Opt.is_set Options.deb_foreign_archs then
    Format.fprintf fmt "foreign-architecture: %s@." (String.concat "," (OptParse.Opt.get Options.deb_foreign_archs));

  if OptParse.Opt.is_set Options.deb_target_arch then
    Format.fprintf fmt "host-architecture: %s@." (OptParse.Opt.get Options.deb_target_arch);

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d = 
    if summary then Diagnostic.collect results d ;
    Diagnostic.fprintf ~pp ~failure ~success ~explain fmt d
  in

  Util.Timer.start timer;
  let i = Depsolver.listcheck ~callback universe checklist in
  ignore(Util.Timer.stop timer ());

  if failure || success then Format.fprintf fmt "@]@.";

  let nb = universe_size in
  let nf = List.length sl in
  Format.fprintf fmt "background-packages: %d@." nb;
  Format.fprintf fmt "foreground-packages: %d@." (if nf = 0 then nb else nf);
  Format.fprintf fmt "broken-packages: %d@." i;

  if summary then
    Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;

  if OptParse.Opt.is_set Options.dump then begin
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file";
    
    Cudf_printer.pp_preamble oc Debcudf.preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc universe
  end

;;

main () ;;
