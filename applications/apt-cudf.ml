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

include Util.Logging(struct let label = "apt-cudf backend" end) ;;

module Options = struct
  open OptParse
  let description = "apt-get backend (EDSP v. 0.4)"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let dump = StdOpt.store_true ()
  let noop = StdOpt.store_true ()
  let solver = StdOpt.str_option ()
  let criteria = StdOpt.str_option ()
  let explain = StdOpt.store_true ()
  let conffile = StdOpt.str_option ~default:"/etc/apt-cudf.conf" ()
  let native_arch = StdOpt.str_option ()
  let foreign_archs = Boilerplate.str_list_option ()

  open OptParser
  add options ~long_name:"conf" ~help:"configuration file (default:/etc/apt-cudf.conf)" conffile;
  add options ~long_name:"dump" ~help:"dump the cudf universe and solution" dump;
  add options ~long_name:"noop" ~help:"Do nothing, implies --dump" noop;
  add options ~short_name:'s' ~long_name:"solver" ~help:"external solver" solver;
  add options ~short_name:'c' ~long_name:"criteria" ~help:"optimization criteria" criteria;
  add options ~short_name:'e' ~long_name:"explain" ~help:"summary" explain;

  add options ~long_name:"native-arch" ~help:"Native architecture" native_arch;
  add options ~long_name:"foreign-archs" ~help:"Foreign architectures" foreign_archs;

end

let fatal fmt =
  Printf.kprintf (fun s ->
    Format.printf "Error: %s (%s)@." 
      (OptParse.Opt.get Options.solver) (OptParse.Opt.get Options.criteria) ;
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

let rec input_all_lines acc chan =
  try input_all_lines ((input_line chan)::acc) chan
  with End_of_file -> acc

let solver_dir = 
  try Sys.getenv("CUDFSOLVERS") with Not_found -> "/usr/share/cudf/solvers"

let apt_get_cmdline = 
  try Sys.getenv("APT_GET_CUDF_CMDLINE") with Not_found -> ""

let pp_pkg fmt (s,univ) = 
  try
    let p = CudfAdd.Cudf_set.choose s in
    let pkg = Hashtbl.find univ (p.Cudf.package,p.Cudf.version) in
    let apt_id = Debian.Packages.assoc "apt-id" pkg.Packages.extras in
    Format.fprintf fmt "%s\n" apt_id;
    Format.fprintf fmt "Package: %s\n" pkg.Packages.name;
    Format.fprintf fmt "Version: %s\n" pkg.Packages.version;
    Format.fprintf fmt "Architecture: %s\n" pkg.Packages.architecture;
  with Not_found -> fatal "apt-cudf internal error"
;;

let pp_pkg_list fmt (l,univ) =
  try 
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
  with Not_found -> fatal "apt-cudf internal error"
;;

let pp_pkg_list_tran fmt (l,univ) = pp_pkg_list fmt (List.map snd l,univ) ;;

(* apt-cudf.conf example :

solver: mccs-cbc , mccs-lpsolve
upgrade: -lex[-new,-removed,-notuptodate]
dist-upgrade: -lex[-notuptodate,-new,-removed]
install: -lex[-removed,-changed]
remove: -lex[-removed,-changed]
trendy: -lex[-removed,-notuptodate,-unsat_recommends,-new]
paranoid: -lex[-removed,-changed]

solver: *
upgrade: -new,-removed,-notuptodate
dist-upgrade: -notuptodate,-new,-removed
install: -removed,-changed
remove: -removed,-changed
trendy: -removed,-notuptodate,-unsat_recommends,-new
paranoid: -removed,-changed
*)
let parse_conf_file fname =
  let pp_lpos { Lexing.pos_fname = _fname;
                pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum } =
    Printf.sprintf "%d:%d" lnum (cnum - bol)
  in
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  try
    let stanzas = Cudf_822_parser.doc_822 Cudf_822_lexer.token_822 lexbuf in
    let r = 
      List.flatten (
        List.map (fun stanza -> 
          let (_,sl) = List.assoc "solver" stanza in
          let l = List.map (fun (k, (_loc, v)) -> (k,v)) stanza in
          List.filter_map (fun s -> 
            match ExtString.String.strip s with
            |"" -> None
            |x -> Some(x,l)
          ) (ExtString.String.nsplit sl ",")
        ) stanzas 
      )
    in
    close_in ic; r
  with Cudf_types.Parse_error_822 (msg, (startpos, endpos)) ->
    fatal "Parse error on file %s:%s--%s" fname (pp_lpos startpos) (pp_lpos endpos)
;;

let choose_criteria ?(criteria=None) ~conffile solver request =
  let conf = 
    if Sys.file_exists conffile then
      parse_conf_file conffile 
    else []
  in
  let default_criteria =
    try List.assoc solver conf
    with Not_found ->
      try List.assoc "*" conf 
      with Not_found -> [
        ("install", "-removed,-changed");
        ("remove", "-removed,-changed");
        ("upgrade","-new,-removed,-notuptodate");
        ("dist-upgrade","-notuptodate,-new,-removed");
        ("trendy","-removed,-notuptodate,-unsat_recommends,-new");
        ("paranoid","-removed,-changed")
      ]
  in
  match criteria,request.Edsp.preferences with
  |None, c when c <> "" -> (try List.assoc c default_criteria with Not_found -> c)
  |Some c,_ when c <> "" -> (try List.assoc c default_criteria with Not_found -> c)
  |_,_ when request.Edsp.upgrade -> List.assoc "upgrade" default_criteria
  |_,_ when request.Edsp.distupgrade -> List.assoc "dist-upgrade" default_criteria
  |_,_ when request.Edsp.install <> [] -> List.assoc "install" default_criteria
  |_,_ when request.Edsp.remove <> [] -> List.assoc "remove" default_criteria
  |_,_ -> List.assoc "paranoid" default_criteria
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

let get_architectures native_opt foreign =
  let cmd = "apt-config dump" in
  let arch = ref "" in
  let archs = ref [] in
  let aux () =
    let out = Std.input_list (Unix.open_process_in cmd) in
    List.iter (fun s ->
      let key, value =  ExtString.String.split s " " in
      if key = "APT::Architecture" then
        arch := ExtString.String.slice ~first: 1 ~last:(-2) value
      else if key = "APT::Architectures::" || key = "APT::Architectures" then
        let s = ExtString.String.slice ~first:1 ~last:(-2) value in
        if s <> "" then
          archs := (ExtString.String.slice ~first:1 ~last:(-2) value)::!archs
    ) out;
    debug "Atomatically set native as %s and foreign archs as %s" !arch (String.concat "," !archs);
  in
  match native_opt, foreign with 
  |None,None     -> aux () ; (!arch,List.filter ((<>) !arch) !archs)
  |None,Some l   -> fatal "Native arch is missing while Foregin archs are specified"
  |Some a,None   -> (a,[])
  |Some a,Some l -> (a,l)
;;

let main () =
  let timer1 = Util.Timer.create "parsing" in
  let timer2 = Util.Timer.create "conversion" in
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers)
  ["parsing";"cudfio";"conversion";"solver";"solution"];
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let (native_arch,foreign_archs) = 
    get_architectures 
      (OptParse.Opt.opt Options.native_arch) 
      (OptParse.Opt.opt Options.foreign_archs)
  in

  let solver = 
    if OptParse.Opt.is_set Options.solver then
      OptParse.Opt.get Options.solver
    else 
      Filename.basename(Sys.argv.(0))
  in
  let exec_pat = fst (parse_solver_spec (Filename.concat solver_dir solver)) in

  let ch = 
    match args with 
    |[] -> (IO.input_channel stdin)
    |file::_ -> Input.open_file file 
  in
  
  Util.Timer.start timer1;
  (* archs are inferred by calling apt-config dump *)
  let archs = native_arch::foreign_archs in
  let (request,pkglist) = Edsp.input_raw_ch ~archs ch in
  let request =
    match apt_get_cmdline with
    |"" -> request
    |_ -> 
      let apt_req = Apt.parse_request_apt apt_get_cmdline in
      Edsp.from_apt_request {request with Edsp.install = []; remove = []} apt_req
  in

  Util.Timer.stop timer1 ();
  
  if args <> [] then Input.close_ch ch;

  Util.Timer.start timer2;
  let tables = Debcudf.init_tables pkglist in
  let default_preamble =
    let l = List.map snd Edsp.extras_tocudf in
    CudfAdd.add_properties Debcudf.preamble l
  in

  let univ = Hashtbl.create (2*(List.length pkglist)-1) in
  let options = {
    Debcudf.default_options with 
    Debcudf.native = native_arch;
    Debcudf.foreign = foreign_archs }
  in 
  let cudfpkglist = 
    List.filter_map (fun pkg ->
      let p = Edsp.tocudf tables ~options pkg in
      if not(Hashtbl.mem univ (p.Cudf.package,p.Cudf.version)) then begin
        Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
        Some p
      end else begin
        warning "Duplicated package (same version, name and architecture) : (%s,%s,%s)" 
          pkg.Packages.name pkg.Packages.version pkg.Packages.architecture;
        None
      end
    ) pkglist 
  in

  let cudfdump = Filename.temp_file "apt-cudf-universe" ".cudf" in
  if OptParse.Opt.get Options.dump || OptParse.Opt.get Options.noop then begin
    Printf.printf "Apt-cudf: dump cudf universe in %s\n" cudfdump;
    let oc = open_out cudfdump in
    Cudf_printer.pp_preamble oc default_preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_packages oc cudfpkglist;
    close_out oc
  end;

  let universe = 
    try Cudf.load_universe cudfpkglist
    with Cudf.Constraint_violation s ->
      fatal "(CUDF) Malformed universe %s" s
  in
  let cudf_request = Edsp.requesttocudf tables universe request in
  let cudf = (default_preamble,universe,cudf_request) in
  Util.Timer.stop timer2 ();

  if OptParse.Opt.get Options.dump || OptParse.Opt.get Options.noop then begin
    Printf.printf "Apt-cudf: append cudf request to %s\n" cudfdump;
    let oc = open_out_gen 
      [Open_wronly; Open_append; Open_creat; Open_text]
      0o666 cudfdump 
    in
    Printf.fprintf oc "\n";
    Cudf_printer.pp_request oc cudf_request;
    close_out oc
  end;

  (* do nothing, we exit here after dumping the universe *)
  if OptParse.Opt.get Options.noop then exit(0);

  let cmdline_criteria = OptParse.Opt.opt Options.criteria in
  let conffile = OptParse.Opt.get Options.conffile in
  let criteria = choose_criteria ~criteria:cmdline_criteria ~conffile solver request in
  OptParse.Opt.set Options.criteria criteria ;

  let solpre,soluniv = 
    let explain = OptParse.Opt.get Options.explain in
    match Algo.Depsolver.check_request ~cmd:exec_pat ~criteria ~explain cudf with
    |Algo.Depsolver.Error s -> fatal "%s" s
    |Algo.Depsolver.Unsat _ -> fatal "(UNSAT) No Solutions according to the give preferences"
    |Algo.Depsolver.Sat s -> s
  in

  if OptParse.Opt.get Options.dump then begin
    let cudfsol = Filename.temp_file "apt-cudf-solution" ".cudf" in
    Printf.printf "Apt-cudf: dump cudf solution in %s\n" cudfsol;
    let oc = open_out cudfsol in
    Cudf_printer.pp_preamble oc default_preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc soluniv;
    close_out oc
  end;

  let diff = CudfDiff.diff universe soluniv in
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
    print_progress ~i:100 "No packages removed or installed";
;;

main ();;

