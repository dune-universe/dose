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
module Boilerplate = BoilerplateNoRpm

let info fmt = Util.make_info "apt-get backend" fmt
let warning fmt = Util.make_warning "apt-get backend" fmt
let debug fmt = Util.make_debug "apt-get backend" fmt
let fatal fmt = Util.make_fatal "apt-get backend" fmt

module Options = struct
  open OptParse
  let description = "apt-get backend (EDSP v. 0.4)"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let dump = StdOpt.store_true ()
  let solver = StdOpt.str_option ()
  let criteria = StdOpt.str_option ()
  let explain = StdOpt.store_true ()

  open OptParser
  add options ~short_name:'d' ~long_name:"dump" ~help:"dump the cudf universe and solution" dump;
  add options ~short_name:'s' ~long_name:"solver" ~help:"external solver" solver;
  add options ~short_name:'c' ~long_name:"criteria" ~help:"optimization criteria" criteria;
  add options ~short_name:'e' ~long_name:"explain" ~help:"summary" explain;

end

let print_error fmt =
  Printf.kprintf (fun s -> 
    Format.printf "Error: %s@." (Util.timestamp ());
    Format.printf "Message: %s@." s;
    exit 0
  ) fmt
;;

let print_progress ?i msg =
  Format.printf "Progress: %s@." (Util.timestamp ());
  if not(Option.is_none i) then
    Format.printf "Percentage: %d@." (Option.get i);
  if msg <> "" then
    Format.printf "Message: %s@." msg
;;

let make_request tables universe request = 
  let constr cs = 
    match CudfAdd.cudfop cs with
    |None -> None
    |Some (c,v) -> Some (c,Debian.Debcudf.get_cudf_version tables ("",v))
  in
  (*** XXX a here is the option architecture *)
  let select_packages l = List.map (fun (n,a,c) -> (n,constr c)) l in
  if request.Edsp.upgrade || request.Edsp.distupgrade then
    let to_upgrade = function
      |[] ->
        let filter pkg = pkg.Cudf.installed in
        let l = Cudf.get_packages ~filter universe in
        List.map (fun pkg -> (pkg.Cudf.package,None)) l
      |l -> select_packages l
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

let pp_pkg_list fmt (l,univ) =
  Format.fprintf fmt "%s" (
    String.concat ", "
    (List.map (fun p ->
      let pkg = Hashtbl.find univ (p.Cudf.package,p.Cudf.version) in
      Printf.sprintf "%s=%s/%s" 
      pkg.Packages.name 
      pkg.Packages.version
      pkg.Packages.architecture
    ) l)
  )
;;

let pp_pkg_list_tran fmt (l,univ) =
  pp_pkg_list fmt (List.map snd l,univ)
;;


(* TODO: add a configuration file to define trendy and paranoid ? *)
let choose_criteria ?(criteria=None) request = 
  let paranoid = "-removed,-changed" in
  let upgrade = "-notuptodate,-removed,-changed" in
  let trendy = "-removed,-notuptodate,-unsat_recommends,-new" in
  match criteria,request.Edsp.preferences with
  |None,"paranoid" when (request.Edsp.upgrade || request.Edsp.distupgrade) -> upgrade
  |None,"paranoid" -> paranoid
  |None,"trendy" -> trendy
  |None,s when s <> "" -> s
  |None,_ when (request.Edsp.upgrade || request.Edsp.distupgrade) -> upgrade
  |None,_ -> paranoid
  |Some "trendy",_ -> trendy
  |Some "paranoid",_ -> paranoid
  |Some "upgrade",_ -> upgrade
  |Some c,_ -> c
;;

let parse_solver_spec filename =
  let (exec, version) = (ref "", ref "") in
  begin try
    let ic = open_in filename in
    while true do
      let l = input_line ic in
      if String.starts_with l "exec: " then
        exec := String.strip (snd (String.split l " "))
      else if String.starts_with l "cudf-version: " then
        Scanf.sscanf l "cudf-version: %s " (fun s -> version := s);
    done;
    close_in ic
    with
      | Sys_error _ -> fatal "cannot parse CUDF solver specification %s" filename
      | End_of_file -> ()
      | Scanf.Scan_failure err ->
        fatal "parse error while reading CUDF solver specification %s: %s"
	  filename err
  end;
  if !exec = "" || !version = "" then
    fatal "incomplete CUDF solver specification %s" filename;
  if not (String.exists !exec "$in" && String.exists !exec "$out"
          && String.exists !exec "$pref") then
    fatal
      "Incomplete solver specification %s: one or more of $in, $out, $pref is missing in exec line"
      filename;
  (!exec,!version)
;;

let check_fail file =
  let ic = open_in file in
  try begin
    let l = input_line ic in
    try (close_in ic ; l = "FAIL")
    with Scanf.Scan_failure _ -> (close_in ic ; false)
  end with End_of_file -> (close_in ic ; false)

(** see mktemp(1) for the syntax of [tmp_pattern] *)
let mktmpdir tmp_pattern =
  let ic =
    Unix.open_process_in (Printf.sprintf "mktemp --tmpdir -d %s" tmp_pattern) in
  let path = input_line ic in
  ignore (Unix.close_process_in ic);
  path

let rmtmpdir path =
  if String.exists path "apt-cudf" then (* safe guard, sort of *)
    ignore (Unix.system (Printf.sprintf "rm -rf %s" path))

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

  (* Solver "exec:" line. Contains three named wildcards to be interpolated:
     "$in", "$out", and "$pref"; corresponding to, respectively, input CUDF
     document, output CUDF universe, user preferences. *)
  let exec_pat =
    if OptParse.Opt.is_set Options.solver then
      let f = OptParse.Opt.get Options.solver in
      Filename.concat solver_dir f
    else
      let f = Filename.basename(Sys.argv.(0)) in
      fst (parse_solver_spec (Filename.concat solver_dir f))
  in
  let interpolate_solver_pat exec cudf_in cudf_out pref =
    let _, exec = String.replace ~str:exec ~sub:"$in"   ~by:cudf_in  in
    let _, exec = String.replace ~str:exec ~sub:"$out"  ~by:cudf_out in
    let _, exec = String.replace ~str:exec ~sub:"$pref" ~by:pref     in
    exec
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
    with Cudf.Constraint_violation s ->
      print_error "(CUDF) Malformed universe %s" s;
  in
  let cudf_request = make_request tables universe request in
  let cudf = (default_preamble,universe,cudf_request) in
  Util.Timer.stop timer2 ();

  let tmpdir = mktmpdir "tmp.apt-cudf.XXXXXXXXXX" in
  at_exit (fun () -> rmtmpdir tmpdir);
  let solver_in = Filename.concat tmpdir "in-cudf" in
  Unix.mkfifo solver_in 0o600;
  let solver_out = Filename.concat tmpdir "out-cudf" in

  let cmdline_criteria = OptParse.Opt.opt (Options.criteria) in
  let criteria = choose_criteria ~criteria:cmdline_criteria request in
  let cmd = interpolate_solver_pat exec_pat solver_in solver_out criteria in
  Printf.eprintf "CMD %s\n%!" cmd;

  let env = Unix.environment () in
  let (cin,cout,cerr) = Unix.open_process_full cmd env in

  Util.Timer.start timer3;
  let solver_in_fd = Unix.openfile solver_in [Unix.O_WRONLY ; Unix.O_SYNC] 0 in
  let oc = Unix.out_channel_of_descr solver_in_fd in
  Cudf_printer.pp_cudf oc cudf;
  close_out oc ;
  Util.Timer.stop timer3 ();

  if OptParse.Opt.get Options.dump then begin
    info "dump universe in  /tmp/cudf-solver.universe.dump";
    let oc = open_out "/tmp/cudf-solver.universe.dump" in
    Cudf_printer.pp_cudf oc cudf;
    close_out oc
  end;

  Util.Timer.start timer4;
  let lines_cin = input_all_lines [] cin in
  let lines = input_all_lines lines_cin cerr in
  let stat = Unix.close_process_full (cin,cout,cerr) in
  begin match stat with
    |Unix.WEXITED 0 -> ()
    |Unix.WEXITED i ->
        print_error "command '%s' failed with code %d" cmd i
    |Unix.WSIGNALED i ->
        print_error "command '%s' killed by signal %d" cmd i
    |Unix.WSTOPPED i ->
        print_error "command '%s' stopped by signal %d" cmd i
  end;
  let debug = String.concat "\n" lines in
  info "%s\n%s\n%!" cmd debug; 
  Util.Timer.stop timer4 ();

  Util.Timer.start timer5;
  if not(Sys.file_exists solver_out) then 
    print_error "(CRASH) Solution file not found"
  else if check_fail solver_out then
    print_error "(UNSAT) No Solutions according to the give preferences"
  else begin
    try begin
      let sol = 
        if (Unix.stat solver_out).Unix.st_size <> 0 then
          let cudf_parser = Cudf_parser.from_file solver_out in
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
        let (i,u,d,r) = CudfDiff.summary universe diff in
        Format.printf "Summary: " ;
        if i <> [] then
          Format.printf "%d to install " (List.length i);
        if r <> [] then
          Format.printf "%d to remove " (List.length r);
        if u <> [] then
          Format.printf "%d to upgrade " (List.length u);
        if d <> [] then
          Format.printf "%d to downgrade " (List.length d);
        Format.printf " @.";

        if i <> [] then
          Format.printf "Installed: %a@." pp_pkg_list (i,univ);
        if r <> [] then 
          Format.printf "Removed: %a@." pp_pkg_list (r,univ);
        if u <> [] then 
          Format.printf "Upgraded: %a@." pp_pkg_list_tran (u,univ);
        if d <> [] then 
          Format.printf "Downgraded: %a@." pp_pkg_list_tran (d,univ);
      end;
      
      if !empty then 
        print_progress ~i:100 "No packages removed or installed"
    end with Cudf.Constraint_violation s ->
      print_error "(CUDF) Malformed solution: %s" s
  end;
  Util.Timer.stop timer5 ();
  Sys.remove solver_in;
  Sys.remove solver_out;
;;

main ();;

