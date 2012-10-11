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
include Util.Logging(struct let label = __FILE__ end) ;;

let check_fail file =
  let ic = open_in file in
  try begin
    let l = input_line ic in
    try (close_in ic ; l = "FAIL")
    with Scanf.Scan_failure _ -> (close_in ic ; false)
  end with End_of_file -> (close_in ic ; false)
;;

(** see mktemp(1) for the syntax of [tmp_pattern] *)
let mktmpdir tmp_pattern =
  let ic =
    Unix.open_process_in (Printf.sprintf "mktemp --tmpdir -d %s" tmp_pattern) in
  let path = input_line ic in
  ignore (Unix.close_process_in ic);
  path

(* XXX this function scares me... what if I manage to create a path = "/" *)
let rmtmpdir path =
  if String.exists path "apt-cudf" then (* safe guard, sort of *)
    ignore (Unix.system (Printf.sprintf "rm -rf %s" path))
;;

let check_exit_status cmd = function
  |Unix.WEXITED 0   -> ()
  |Unix.WEXITED i   -> fatal "command '%s' failed with code %d" cmd i
  |Unix.WSIGNALED i -> fatal "command '%s' killed by signal %d" cmd i
  |Unix.WSTOPPED i  -> fatal "command '%s' stopped by signal %d" cmd i
;;

let rec input_all_lines acc chan =
  try input_all_lines ((input_line chan)::acc) chan
  with End_of_file -> acc
;;

(** Solver "exec:" line. Contains three named wildcards to be interpolated:
   "$in", "$out", and "$pref"; corresponding to, respectively, input CUDF
   document, output CUDF universe, user preferences. *)
let interpolate_solver_pat exec cudf_in cudf_out pref =
  let _, exec = String.replace ~str:exec ~sub:"$in"   ~by:cudf_in  in
  let _, exec = String.replace ~str:exec ~sub:"$out"  ~by:cudf_out in
  let _, exec = String.replace ~str:exec ~sub:"$pref" ~by:pref     in
  exec
;;

exception Error of string
exception Unsat

let fatal fmt =
  Printf.kprintf (fun s ->
    raise (Error s)
  ) fmt
;;

(** [execsolver] execute an external cudf solver.
    exec_pat : execution string
    cudf : a cudf document (preamble, universe, request)
    criteria : optimization criteria
*)
let execsolver exec_pat criteria cudf = 
  let timer3 = Util.Timer.create "cudfio" in
  let timer4 = Util.Timer.create "solver" in
  let (_,universe,_) = cudf in

  let tmpdir = mktmpdir "tmp.apt-cudf.XXXXXXXXXX" in
  at_exit (fun () -> rmtmpdir tmpdir);
  let solver_in = Filename.concat tmpdir "in-cudf" in
  Unix.mkfifo solver_in 0o600;
  let solver_out = Filename.concat tmpdir "out-cudf" in
  let cmd = interpolate_solver_pat exec_pat solver_in solver_out criteria in

  debug "%s" cmd;

  let env = Unix.environment () in
  let (cin,cout,cerr) = Unix.open_process_full cmd env in

  Util.Timer.start timer3;
  let solver_in_fd = Unix.openfile solver_in [Unix.O_WRONLY ; Unix.O_SYNC] 0 in
  let oc = Unix.out_channel_of_descr solver_in_fd in
  Cudf_printer.pp_cudf oc cudf;
  close_out oc ;
  Util.Timer.stop timer3 ();

  Util.Timer.start timer4;
  let lines_cin = input_all_lines [] cin in
  let lines = input_all_lines lines_cin cerr in
  let exit_code = Unix.close_process_full (cin,cout,cerr) in
  check_exit_status cmd exit_code;
  info "%s!!" cmd;
  debug "\n%s" (String.concat "\n" lines);
  Util.Timer.stop timer4 ();

  if not(Sys.file_exists solver_out) then
    fatal "(CRASH) Solution file not found"
  else if check_fail solver_out then
    raise Unsat
  else 
    try begin
      if (Unix.stat solver_out).Unix.st_size <> 0 then
        let cudf_parser = Cudf_parser.from_file solver_out in
        Sys.remove solver_in; Sys.remove solver_out ;
        try Cudf_parser.load_solution cudf_parser universe with
        |Cudf_parser.Parse_error _
        |Cudf.Constraint_violation _ ->
          fatal "(CRASH) Solution file contains an invalid solution"
      else fatal "(CRASH) Solution file is empty"
   end with Cudf.Constraint_violation s ->
     fatal "(CUDF) Malformed solution: %s" s ;
;;
