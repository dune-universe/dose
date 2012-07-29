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
  (* let checkonly = Boilerplate.vpkglist_option () *)
  let summary = StdOpt.store_true ()
  let dump = StdOpt.str_option ()
  let host = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  (* add options ~long_name:"checkonly" ~help:"Check only these package" checkonly; *)
  add options ~long_name:"summary" ~help:"Print a detailed summary" summary;

  add options ~long_name:"dump" ~help:"dump the cudf file" dump;

  add options ~long_name:"host" ~help:"cross build for foreign architecture" host;

  include Boilerplate.MakeDistribOptions(struct let options = options end);;

end

include Util.Logging(struct let label = __FILE__ end) ;;

let timer = Util.Timer.create "Solver"

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];

  if not(OptParse.Opt.is_set Options.deb_native_arch) then 
      fatal "your must specify at least the native architecture";

  (*
   let options = Options.set_options (Input.guess_format [posargs]) in
*)

  let archs = if OptParse.Opt.is_set Options.host then
    (OptParse.Opt.get Options.host)::
      (OptParse.Opt.get Options.deb_foreign_arch)
  else
    (OptParse.Opt.get Options.deb_native_arch)::
      (OptParse.Opt.get Options.deb_foreign_arch)
  in

  let options = if OptParse.Opt.is_set Options.host then
    { Debcudf.default_options with
      Debcudf.native = OptParse.Opt.get Options.deb_native_arch;
      Debcudf.foreign = OptParse.Opt.get Options.deb_foreign_arch;
      Debcudf.host = OptParse.Opt.get Options.host;
    }
  else
    Debcudf.default_options
  in

  let pkglist, srclist =
    match posargs with
    |[] | [_] -> fatal 
      "You must provide a list of Debian Packages files and \
       a Debian Sources file"
    |l -> 
        begin match List.rev l with
        |h::t ->
          let srclist =
            let l = Src.input_raw [h] in
            Src.sources2packages archs l
          in
          let pkglist = Deb.input_raw t in
          (pkglist,srclist)
        |_ -> fatal "An impossible situation occurred ?!#"
        end
  in
  let tables = Debcudf.init_tables (srclist @ pkglist) in
 
  let pp pkg =
    let (p,i) = (pkg.Cudf.package,pkg.Cudf.version) in
    let v = Debian.Debcudf.get_real_version tables (p,i) in
    let l =
      List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["architecture";"source";"sourcenumber"]
    in (p,v,l)
  in
  let sl = List.map (fun pkg -> Debcudf.tocudf ~options tables pkg) srclist in
  let l = List.fold_left (fun acc pkg -> (Debcudf.tocudf ~options tables pkg)::acc) sl pkglist in

  let universe = Cudf.load_universe l in
  let universe_size = Cudf.universe_size universe in

  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let summary = OptParse.Opt.get Options.summary in
  let fmt = Format.std_formatter in

  let results = Diagnostic.default_result universe_size in

  Format.fprintf fmt "native-architecture: %s@." (OptParse.Opt.get Options.deb_native_arch);

  if (OptParse.Opt.get Options.deb_foreign_arch) != [] then
    Format.fprintf fmt "foreign-architecture: %s@." (String.concat "," (OptParse.Opt.get Options.deb_foreign_arch));

  if OptParse.Opt.is_set Options.host then
    Format.fprintf fmt "cross build for host-architecture: %s@." (OptParse.Opt.get Options.host);

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d = 
    if summary then Diagnostic.collect results d ;
    Diagnostic.fprintf ~pp ~failure ~success ~explain fmt d
  in

  Util.Timer.start timer;
  let i = Depsolver.listcheck ~callback universe sl in
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
    Cudf_printer.pp_universe oc universe
  end

;;

main () ;;
