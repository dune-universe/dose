(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

let info fmt = Util.make_info "apt-get backend" fmt
let warning fmt = Util.make_warning "apt-get backend" fmt
let debug fmt = Util.make_debug "apt-get backend" fmt
let fatal fmt = Util.make_fatal "apt-get backend" fmt

module Options = struct
  open OptParse
  let description = "apt-get backend (EDSP v. 0.3)"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let outfile = StdOpt.str_option ()
  let solfile = StdOpt.str_option ()
  let solver = StdOpt.str_option ()

  open OptParser
  add options ~long_name:"univfile" ~help:"dump the cudf universe" outfile;
  add options ~long_name:"solfile" ~help:"dump the cudf solution" solfile;
  add options ~short_name:'s' ~long_name:"solver" ~help:"external solver" solver;

end

let norm s = Str.string_before s (String.index s ':') ;;
let make_request request = 
  (* XXX add here the semantic translation for autoremove, strict-pinning
   * and friends in req_extra *)
  {Cudf.default_request with
  Cudf.request_id = request.Edsp.request;
  Cudf.install = List.map (fun (n,c) -> (norm n,None)) request.Edsp.install;
  Cudf.remove = List.map (fun (n,c) -> (norm n,None)) request.Edsp.remove;
  }
;;

let rec input_all_lines acc chan =
  try input_all_lines ((input_line chan)::acc) chan
  with End_of_file -> acc

let exec cmd =
  let env = Unix.environment () in
  let (cin,cout,cerr) = Unix.open_process_full cmd env in
  let lines_cin = input_all_lines [] cin in
  let lines = input_all_lines lines_cin cerr in
  let stat = Unix.close_process_full (cin,cout,cerr) in
  begin match stat with
    |Unix.WEXITED 0 -> ()
    |Unix.WEXITED i ->
        info "command '%s'\nfailed with code %d" cmd i
    |Unix.WSIGNALED i ->
        info "command '%s'\nkilled by signal %d" cmd i 
    |Unix.WSTOPPED i ->
        info "command '%s'\nstopped by signal %d" cmd i
  end;
  String.concat "\n" lines
;;

let solver_dir = 
  try Sys.getenv("CUDFSOLVERS") with Not_found -> "/usr/lib/cudf/solvers"

let pp_pkg fmt (s,univ) = 
  let p = CudfAdd.Cudf_set.choose s in
  let pkg = Hashtbl.find univ (p.Cudf.package,p.Cudf.version) in
  let apt_id = Debian.Packages.assoc "APT-ID" pkg.Packages.extras in
  Format.fprintf fmt "%s\n" apt_id;
  Format.fprintf fmt "Package: %s\n" pkg.Packages.name;
  Format.fprintf fmt "Version: %s\n" pkg.Packages.version;
  Format.fprintf fmt "Architecture: %s\n" pkg.Packages.architecture;
;;

(* TODO: the criteria should be computed on the fly w.r.t. strict pinning and
 * preferences *)
let choose_criteria request = 
  let paranoid = "-removed,-changed" in
  let upgrade = "-notuptodate,-removed,-changed" in
  let trendy = "-removed,-notuptodate,-unmet_recommends,-new" in
  if request.Edsp.upgrade || request.Edsp.distupgrade then
    upgrade
  else
    paranoid
;;

let main () =
  let timer1 = Util.Timer.create "parsing" in
  let timer2 = Util.Timer.create "conversion" in
  let timer3 = Util.Timer.create "cudfio" in
  let timer4 = Util.Timer.create "solver" in
  let timer5 = Util.Timer.create "solution" in
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers)
  ["parsing";"cudfio";"conversion";"solver";"solution"];

  let solver =
    let name = 
      if OptParse.Opt.is_set Options.solver then
        OptParse.Opt.get Options.solver
      else
        Filename.basename(Sys.argv.(0))
    in 
    Filename.concat (Filename.concat solver_dir name) name
  in

  let ch = 
    match args with 
    |[] -> (IO.input_channel stdin)
    |file::_ -> Input.open_file file 
  in
  
  Util.Timer.start timer1;
  let (request,pkglist) = Edsp.input_raw_ch ch in
  Util.Timer.stop timer1 ();
  
  if args <> [] then Input.close_ch ch;

  Util.Timer.start timer2;
  let tables = Debcudf.init_tables pkglist in
  let default_preamble =
    let l = List.map snd Edsp.extras_tocudf in
    CudfAdd.add_properties Debcudf.preamble l
  in
  
  let univ = Hashtbl.create (2*(List.length pkglist)-1) in
  let cudfpkglist = 
    List.map (fun pkg ->
      let p = Edsp.tocudf tables pkg in
      Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
      p
    ) pkglist 
  in
  let universe = Cudf.load_universe cudfpkglist in
  let cudf_request = make_request request in
  let cudf = (default_preamble,universe,cudf_request) in
  Util.Timer.stop timer2 ();
  (*
  let oc =
    if OptParse.Opt.is_set Options.outfile then
      open_out (OptParse.Opt.get Options.outfile)
    else
      stdout
  in
  *)

  (* XXX we should use a named pipe for in and out or a tmp file *)
  let infile = "/tmp/cudf_req" in
  let outfile = "/tmp/cudf_sol" in

  let oc = open_out infile in
  
  Util.Timer.start timer3;
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a" Cudf_printer.pp_cudf cudf;
  if oc <> stdout then close_out oc ;
  Util.Timer.stop timer3 ();

  Util.Timer.start timer4;
  let criteria = choose_criteria request in
  let cmd = Printf.sprintf "%s %s %s %s" solver infile outfile criteria in
  let debug = exec cmd in
  info "%s\n%s\n%!" cmd debug; 
  Util.Timer.stop timer4 ();

  Util.Timer.start timer5;
  let cudf_parser = Cudf_parser.from_file outfile in
  let (_,sol,_) = Cudf_parser.parse cudf_parser in
  let diff = CudfDiff.diff (Cudf.load_universe cudfpkglist) (Cudf.load_universe sol) in
  Hashtbl.iter (fun pkgname s ->
    let inst = s.CudfDiff.installed in
    let rem = s.CudfDiff.removed in
    match CudfAdd.Cudf_set.is_empty inst, CudfAdd.Cudf_set.is_empty rem with
    |false,true ->
        Format.printf "Install: %a@." pp_pkg (inst,univ)
    |true,false ->
        Format.printf "Remove: %a@." pp_pkg (rem,univ)
    |false,false ->
        Format.printf "Remove: %a@." pp_pkg (rem,univ);
        Format.printf "Install: %a@." pp_pkg (inst,univ)
    |true,true -> ()
  ) diff;
  Util.Timer.stop timer5 ();
;;

main ();;

