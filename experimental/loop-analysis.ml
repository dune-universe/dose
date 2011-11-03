(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  ADD authors here                                     *)
(*                                                                        *)
(*  Contributor(s):  ADD minor contributors here                          *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common
open Debian
open Algo

let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt
let debug fmt = Util.make_debug __FILE__ fmt
let fatal fmt = Util.make_fatal __FILE__ fmt

module Boilerplate = BoilerplateNoRpm
module Src = Sources
module Pkg = Packages

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"Detect circular build dependencies"
  include Boilerplate.MakeOptions(struct let options = options end)

  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let summary = StdOpt.store_true ()

  let checkonly = Boilerplate.vpkglist_option ()
  let buildarch = StdOpt.str_option ()
  let targetarch = StdOpt.str_option ()
  let dump = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;

  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;
  add options ~long_name:"summary" ~help:"Print a detailed summary" summary;

  add options ~long_name:"builarch" ~help:"Build Architecture" buildarch;
  add options ~long_name:"targetarch" ~help:"Target Architecture" targetarch;
  add options ~long_name:"dump" ~help:"dump the cudf file" dump;
end

let tocudf tables s2p pkg =
  let extras = [("srctype",("srctype",`String None))] in
  let is_src s = 
    try (List.assoc "srctype" pkg.Pkg.extras) = s 
    with Not_found -> false in
  if is_src "src" then
    let provides = CudfAdd.get_package_list s2p pkg.Pkg.name in
    let depends =
      List.map (fun l ->
        List.map (fun (n,c) -> ("bin"^Src.sep^n,c)
        ) l
      ) (pkg.Pkg.pre_depends @ pkg.Pkg.depends)
    in
    let p = {
      Pkg.default_package with
      Pkg.name = pkg.Pkg.name;
      Pkg.version = pkg.Pkg.version;
      Pkg.provides = List.map (fun n -> ("bin"^Src.sep^n,None) ) provides;
      Pkg.depends = depends
    }
    in Debcudf.tocudf tables ~extras p
  else
    let confl = pkg.Pkg.breaks @ pkg.Pkg.conflicts in
    let deps =
      let d = pkg.Pkg.pre_depends @ pkg.Pkg.depends in
      if is_src "srcf" then d
      else 
        let (sn,sv) =
          match pkg.Pkg.source with
          |("",None) -> ("src" ^ Src.sep ^ pkg.Pkg.name,Some("=",pkg.Pkg.version))
          |(s,None) -> ("src" ^ Src.sep ^ s,Some("=",pkg.Pkg.version))
          |(s,Some v) -> ("src" ^ Src.sep ^ s,Some("=",v))
        in
        [(sn,None)]::d
    in
    let p = { pkg with Pkg.depends = deps ; Pkg.conflicts = confl } in
    Debcudf.tocudf tables ~extras p
;;

let main () =

  let posargs = OptParse.OptParser.parse_argv Options.options in
  
  (* enable info / warning / debug information *)
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  
  (* enable a selection of progress bars *)
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;

  (* enable a selection of timers *)
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];

  if not(OptParse.Opt.is_set Options.buildarch) then
    fatal "--builarch must be specified";

  if not(OptParse.Opt.is_set Options.targetarch) then begin
    info "--targetarch must be specified assume same of buildarch";
    OptParse.Opt.set Options.targetarch (OptParse.Opt.get Options.buildarch);
  end;

  let binlist, srclist, srcfocus =
    match posargs with
    |[] | [_] -> fatal
      "You must provide a list of Debian Packages files and \
       a Debian Sources file"
    |l ->
        begin match List.rev l with
        |h::t ->
          let l = Src.input_raw [h] in
          let archs = ["linux-any";OptParse.Opt.get Options.buildarch] in
          let srcl = Src.sources2packages archs l in
          let srcf = Src.sources2packages ~src:"srcf" archs l in
          let pkgl = Pkg.input_raw t in
          (pkgl,srcl,srcf)
        |_ -> failwith "Impossible"
        end
  in

  let src2provds = Hashtbl.create (List.length srclist) in
  List.iter (fun pkg ->
    List.iter (fun (n,_) -> 
      let (source,_) = Debutil.get_source pkg in 
      CudfAdd.add_to_package_list src2provds ("src"^Src.sep^source) n
    ) ((pkg.Pkg.name,None)::pkg.Pkg.provides)
  ) binlist;

  let tables = Debcudf.init_tables (srclist @ binlist) in
  let tocudf__ = tocudf tables src2provds in
  let sl = List.map (fun pkg -> tocudf__ pkg) srclist in
  let sf = List.fold_left (fun acc pkg -> (tocudf__ pkg)::acc) sl srcfocus in
  let pkglist = List.fold_left (fun acc pkg -> (tocudf__ pkg)::acc) sf binlist in

  let to_cudf = Debcudf.get_cudf_version tables in
  let from_cudf = Debcudf.get_real_version tables in

  let universe = Cudf.load_universe pkglist in

  let universe_size = Cudf.universe_size universe in
  info "Total packages (source + binaries) %d" universe_size;

  if OptParse.Opt.is_set Options.dump then begin
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file";
    Cudf_printer.pp_universe oc universe
  end;

  let checklist =
    if OptParse.Opt.is_set Options.checkonly then
        List.flatten (
          List.map (function
            |(p,None) ->
                Cudf.lookup_packages universe (CudfAdd.encode ("srcf"^Src.sep^p))
            |(p,Some(c,v)) ->
                let filter = Some(c,to_cudf (CudfAdd.encode ("srcf"^Src.sep^p),v)) in
                Cudf.lookup_packages ~filter universe (CudfAdd.encode ("srcf"^Src.sep^p))
          ) (OptParse.Opt.get Options.checkonly)
        )
    else sf
  in

  let failure = OptParse.Opt.get Options.failures in
  let success = OptParse.Opt.get Options.successes in
  let explain = OptParse.Opt.get Options.explain in
  let summary = OptParse.Opt.get Options.summary in
  let fmt = Format.std_formatter in

  let results = Diagnostic.default_result universe_size in

  if OptParse.Opt.is_set Options.buildarch then
    Format.fprintf fmt "buildarch: %s@." (OptParse.Opt.get Options.buildarch);
  if OptParse.Opt.is_set Options.targetarch then
    Format.fprintf fmt "targetarch: %s@." (OptParse.Opt.get Options.targetarch);

  let pp pkg =
    let p = pkg.Cudf.package in
    let v = from_cudf (CudfAdd.decode pkg.Cudf.package,pkg.Cudf.version) in
    let l =
      List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["architecture";"source";"sourceversion"]
    in (p,v,l)
  in

  if failure || success then Format.fprintf fmt "@[<v 1>report:@,";
  let callback d =
    if summary then Diagnostic.collect results d ;
    Diagnostic.fprintf ~pp ~failure ~success ~explain fmt d
  in

  let i = Depsolver.listcheck ~callback universe checklist in

  if failure || success then Format.fprintf fmt "@]@.";

  let nb = universe_size in
  let nf = List.length sl in
  Format.fprintf fmt "background-packages: %d@." nb;
  Format.fprintf fmt "foreground-packages: %d@." (if nf = 0 then nb else nf);
  Format.fprintf fmt "broken-packages: %d@." i;

  if summary then
    Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results
;;

main ();;

