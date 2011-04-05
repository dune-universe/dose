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
  add options ~long_name:"outfile" ~help:"universe output" outfile;
  add options ~long_name:"solfile" ~help:"solution output" solfile;
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
        fatal "command '%s'\nfailed with code %d\n" cmd i
    |Unix.WSIGNALED i ->
        fatal "command '%s'\nkilled by signal %d\n" cmd i 
    |Unix.WSTOPPED i ->
        fatal "command '%s'\nstopped by signal %d" cmd i
  end;
  String.concat "\n" lines
;;

let solver_dir = 
  try Sys.getenv("APTSOLVERS") with Not_found -> "/usr/lib/apt/solvers"

let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];

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
  
  let (request,pkglist) = Edsp.input_raw_ch ch in
  
  if args <> [] then Input.close_ch ch;

  let tables = Debcudf.init_tables pkglist in
  let default_preamble =
    let l = List.map snd Edsp.extras_tocudf in
    CudfAdd.add_properties Debcudf.preamble l
  in
  
  let univ = Hashtbl.create (2*(List.length pkglist)-1) in
  let cudfpkglist = 
    List.map (fun pkg ->
      let p = Edsp.tocudf tables pkg in
      Hashtbl.add univ (p.Cudf.package,p.Cudf.version) (p.Cudf.installed,pkg);
      p
    ) pkglist 
  in
  (*
  let oc =
    if OptParse.Opt.is_set Options.outfile then
      open_out (OptParse.Opt.get Options.outfile)
    else
      stdout
  in
  *)

  (* XXX we should use a named pipe for in and out *)
  let infile = "/tmp/cudf_req" in
  let outfile = "/tmp/cudf_sol" in

  let oc = open_out infile in
  
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@\n" Cudf_printer.pp_preamble default_preamble;
  List.iter (Format.fprintf fmt "%a@\n" Cudf_printer.pp_package) cudfpkglist ;
  Format.fprintf fmt "%a@\n" Cudf_printer.pp_request (make_request request);
  if oc <> stdout then close_out oc ;

  (* TODO: the criteria should be computed on the fly w.r.t. strict pinning and
   * preferences *)
  let criteria = "-notuptodate,-removed,-changed" in
  let cmd = Printf.sprintf "%s %s %s %s" solver infile outfile criteria in
  let debug = exec cmd in
  Printf.eprintf "%s\n%s\n%!" cmd debug; 

  let cudf_parser = Cudf_parser.from_file outfile in
  let (_,sol,_) = Cudf_parser.parse cudf_parser in
  (* List.iter (Format.printf "%a@." Cudf_printer.pp_package) sol; *)
  List.iter (fun p ->
    try
      let (was_inst,pkg) = Hashtbl.find univ (p.Cudf.package,p.Cudf.version) in
      let apt_id = List.assoc "apt-id" pkg.Packages.extras in
      match p.Cudf.installed,was_inst with
      |true,true | false,false -> ()
      |true,false -> Format.printf "Install: %s@." apt_id
      |false,true -> Format.printf "Remove: %s@." apt_id
    with Not_found -> fatal "Conversion error"
  ) sol

;;

main ();;

