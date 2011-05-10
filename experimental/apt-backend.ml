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
  let criteria = StdOpt.str_option ()
  let explain = StdOpt.store_true ()

  open OptParser
  add options ~long_name:"univfile" ~help:"dump the cudf universe" outfile;
  add options ~long_name:"solfile" ~help:"dump the cudf solution" solfile;
  add options ~short_name:'s' ~long_name:"solver" ~help:"external solver" solver;
  add options ~short_name:'c' ~long_name:"criteria" ~help:"optimization criteria" criteria;
  add options ~short_name:'e' ~long_name:"explain" ~help:"summary" explain;

end

(* XXX : Multi-arch Hack *)
let norm s = 
  try Str.string_before s (String.index s ':') 
  with Not_found -> s
;;

let make_request universe request = 
  let select_packages l = List.map (fun (n,c) -> (norm n,None)) l in
  if request.Edsp.upgrade || request.Edsp.distupgrade then
    let to_upgrade = function
      |[] ->
        let filter pkg = pkg.Cudf.installed in
        let l = Cudf.get_packages ~filter universe in
        List.map (fun pkg -> (pkg.Cudf.package,None)) l
      |l -> List.map (fun (n,c) -> (norm n,None)) l
    in
    {Cudf.default_request with 
    Cudf.request_id = request.Edsp.request;
    Cudf.upgrade = to_upgrade request.Edsp.install;
    Cudf.remove = select_packages request.Edsp.remove;
    }
  else
    {Cudf.default_request with
    Cudf.request_id = request.Edsp.request;
    Cudf.install = select_packages request.Edsp.install;
    Cudf.remove = select_packages request.Edsp.remove;
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
  try Sys.getenv("CUDFSOLVERS") with Not_found -> "/usr/share/cudf/solvers"

let pp_pkg fmt (s,univ) = 
  let p = CudfAdd.Cudf_set.choose s in
  let pkg = Hashtbl.find univ (p.Cudf.package,p.Cudf.version) in
  let apt_id = Debian.Packages.assoc "APT-ID" pkg.Packages.extras in
  Format.fprintf fmt "%s\n" apt_id;
  Format.fprintf fmt "Package: %s\n" pkg.Packages.name;
  Format.fprintf fmt "Version: %s\n" pkg.Packages.version;
  Format.fprintf fmt "Architecture: %s\n" pkg.Packages.architecture;
;;

(* TODO: strict pinning *)
(* TODO: add a configuration file to define trendy and paranoid ? *)
let choose_criteria ?(criteria=None) request = 
  let paranoid = "-removed,-changed" in
  let upgrade = "-notuptodate,-removed,-changed" in
  let trendy = "-removed,-notuptodate,-unsat_recommends,-new" in
  match criteria,request.Edsp.preferences with
  |None,s when s <> "" -> s
  |None,_ when (request.Edsp.upgrade || request.Edsp.distupgrade) -> upgrade
  |None,_ -> paranoid
  |Some "trendy",_ -> trendy
  |Some "paranoid",_ -> paranoid
  |Some "upgrade",_ -> upgrade
  |Some c,_ -> c
;;

let parse_solver filename =
  let name = ref "" in
  let version = ref "" in
  let ic = open_in filename in
  begin try while true do
    let l = input_line ic in
    try 
      Scanf.sscanf l "exec: %s " (fun s -> name := s);
      Scanf.sscanf l "cudf-version: %s " (fun s -> version := s);
    with Scanf.Scan_failure _ -> ()
  done with End_of_file -> () end;
  close_in ic;
  if !name = "" then fatal "cannot read %s" filename
  else (!name,!version)
;;

let check_fail s =
  let ic = open_in s in
  try begin
    let l = input_line ic in
    try l = "FAIL"
    with Scanf.Scan_failure _ -> false
  end with End_of_file -> false

let timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
;;


let print_error s =
  Format.printf "Error: %s@." (timestamp ());
  Format.printf "Message: %s@." s;
  exit 0
;;

let print_progress ?i msg =
  Format.printf "Progress: %s@." (timestamp ());
  if not(Option.is_none i) then
    Format.printf "Percentage: %d@." (Option.get i);
  if msg <> "" then
    Format.printf "Message: %s@." msg
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
    if OptParse.Opt.is_set Options.solver then
      let f = OptParse.Opt.get Options.solver in
      Filename.concat solver_dir f
    else
      let f = Filename.basename(Sys.argv.(0)) in
      fst(parse_solver (Filename.concat solver_dir f))
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
  let universe = 
    try Cudf.load_universe cudfpkglist
    with Cudf.Constraint_violation s -> begin
      print_error ("(CUDF) Malformed universe "^s);
      exit 0
    end
  in
  let cudf_request = make_request universe request in
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
  let cmdline_criteria = OptParse.Opt.opt (Options.criteria) in
  let criteria = choose_criteria ~criteria:cmdline_criteria request in
  let cmd = Printf.sprintf "%s %s %s %s" solver infile outfile criteria in
  let debug = exec cmd in
  info "%s\n%s\n%!" cmd debug; 
  Util.Timer.stop timer4 ();

  Util.Timer.start timer5;
  if not(Sys.file_exists outfile) then 
    print_error "(CRASH) Solution file not found"
  else if check_fail outfile then
    print_error "(UNSAT) No Solutions according to the give preferences"
  else begin
    try begin
      let sol = 
        if (Unix.stat outfile).Unix.st_size <> 0 then
          let cudf_parser = Cudf_parser.from_file outfile in
          let (_,sol,_) = Cudf_parser.parse cudf_parser in
          sol
        else []
      in
      let diff = CudfDiff.diff universe (Cudf.load_universe sol) in
      let empty = ref true in
      Hashtbl.iter (fun pkgname s ->
        let inst = s.CudfDiff.installed in
        let rem = s.CudfDiff.removed in
        match CudfAdd.Cudf_set.is_empty inst, CudfAdd.Cudf_set.is_empty rem with
        |false,true -> begin
            empty := false;
            Format.printf "Install: %a@." pp_pkg (inst,univ)
        end
        |true,false -> begin
            empty := false;
            Format.printf "Remove: %a@." pp_pkg (rem,univ)
        end
        |false,false -> begin
            empty := false;
            Format.printf "Remove: %a@." pp_pkg (rem,univ);
            Format.printf "Install: %a@." pp_pkg (inst,univ)
        end
        |true,true -> ()
      ) diff;

      if OptParse.Opt.get Options.explain then begin
        Format.printf "Summary:";
        Format.printf "Unchanged: %d" 0;
        Format.printf "Installed: (%d) %s" 0 "";
        Format.printf "Removed: (%d) %s" 0 "";
        Format.printf "Upgraded: (%d) %s" 0 "";
        Format.printf "Downgraded: (%d) %s" 0 "";
      end;
        
      if !empty then 
        print_progress ~i:100 "No packages removed or installed"
        (*
      else
        print_progress ~i:100 ""
        *)
    end with Cudf.Constraint_violation s ->
      print_error ("(CUDF) Malformed solution: "^s)
  end;
  Util.Timer.stop timer5 ();
;;

main ();;

