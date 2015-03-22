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

module Pcre = Re_pcre

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

let prng = lazy(Random.State.make_self_init ());;

(* bits and pieces borrowed from ocaml stdlib/filename.ml *)
let mktmpdir prefix suffix =
  let temp_dir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  let temp_file_name temp_dir prefix suffix =
    let rnd = (Random.State.bits (Lazy.force prng)) land 0xFFFFFF in
    Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)
  in
  let rec try_name counter =
    let name = temp_file_name temp_dir prefix suffix in
    try
      Unix.mkdir name 0o700;
      name
    with Unix.Unix_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0

let rmtmpdir path =
  begin try
    Sys.remove (Filename.concat path "in-cudf");
    Sys.remove (Filename.concat path "out-cudf")
  with e -> () end;
  try
    Unix.rmdir path
  with e ->
    warning "cannot delete temporary directory %s - not empty?" path;
    raise e
;;

let rec input_all_lines acc chan =
  try input_all_lines ((input_line chan)::acc) chan
  with End_of_file -> acc
;;

(** Solver "exec:" line. Contains three named wildcards to be interpolated:
   "$in", "$out", and "$pref"; corresponding to, respectively, input CUDF
   document, output CUDF universe, user preferences. *)

(* quote string for the shell, after removing all characters disallowed in criteria *)
let quote s = Printf.sprintf "\"%s\"" s;;
let sanitize s = quote (Pcre.substitute ~rex:(Pcre.regexp "[^+()a-z,\"-]") ~subst:(fun _ -> "") s);;

let interpolate_solver_pat exec cudf_in cudf_out pref =
  let (|>) f g = fun x -> g(f x) in
  let dequote s = Pcre.regexp ("\"*"^(Pcre.quote s)^"\"*") in
  (Pcre.substitute ~rex:(dequote "$in")   ~subst: (fun _ -> (quote cudf_in))  |>
   Pcre.substitute ~rex:(dequote "$out")  ~subst: (fun _ -> (quote cudf_out)) |>
   Pcre.substitute ~rex:(dequote "$pref") ~subst: (fun _ -> (sanitize pref))) exec
;;

exception Error of string
exception Unsat

let fatal fmt =
  Printf.kprintf (fun s ->
    raise (Error s)
  ) fmt
;;

let check_exit_status cmd = function
  |Unix.WEXITED 0   -> ()
  |Unix.WEXITED i   -> fatal "command '%s' failed with code %d" cmd i
  |Unix.WSIGNALED i -> fatal "command '%s' killed by signal %d" cmd i
  |Unix.WSTOPPED i  -> fatal "command '%s' stopped by signal %d" cmd i
;;

let timer3 = Util.Timer.create "cudfio" ;;
let timer4 = Util.Timer.create "solver" ;;

(** [execsolver] execute an external cudf solver.
    exec_pat : execution string
    cudf : a cudf document (preamble, universe, request)
    criteria : optimization criteria
*)
let execsolver exec_pat criteria cudf = 
  let (_,universe,_) = cudf in

  let tmpdir = mktmpdir "tmp.apt-cudf." "" in
  at_exit (fun () -> rmtmpdir tmpdir);
  let solver_in = Filename.concat tmpdir "in-cudf" in
  Unix.mkfifo solver_in 0o600;
  let solver_out = Filename.concat tmpdir "out-cudf" in
  let cmd = interpolate_solver_pat exec_pat solver_in solver_out criteria in

  notice "%s" cmd;

  (* Tell OCaml we want to capture SIGCHLD                       *)
  (* In case the external solver fails before reading its input, *)
  (* this will raise a Unix.EINTR error which is captured below  *)
  let eintr_handl = Sys.signal Sys.sigchld (Sys.Signal_handle (fun _ -> ())) in

  let env = Unix.environment () in
  let (cin,cout,cerr) = Unix.open_process_full cmd env in

  Util.Timer.start timer3;
  begin
    try
      let solver_in_fd = Unix.openfile solver_in [Unix.O_WRONLY;Unix.O_SYNC] 0 in
      let oc = Unix.out_channel_of_descr solver_in_fd in
      Cudf_printer.pp_cudf oc cudf;
      close_out oc
    with Unix.Unix_error (Unix.EINTR,_,_) ->  info "Interrupted by EINTR while executing command '%s'" cmd
  end;
  Util.Timer.stop timer3 ();
  (* restore previous behaviour on sigchild *)
  Sys.set_signal Sys.sigchld eintr_handl;

  Util.Timer.start timer4;
  let lines_cin = input_all_lines [] cin in
  let lines = input_all_lines lines_cin cerr in
  let exit_code = Unix.close_process_full (cin,cout,cerr) in
  check_exit_status cmd exit_code;
  notice "\n%s" (String.concat "\n" lines);
  Util.Timer.stop timer4 ();

  if not(Sys.file_exists solver_out) then
    fatal "(CRASH) Solution file not found"
  else if check_fail solver_out then
    raise Unsat
  else 
    try begin
      let cudf_parser = Cudf_parser.from_file solver_out in
      try Cudf_parser.load_solution cudf_parser universe with
      |Cudf_parser.Parse_error _
      |Cudf.Constraint_violation _ ->
        fatal "(CRASH) Solution file contains an invalid solution"
   end with Cudf.Constraint_violation s ->
     fatal "(CUDF) Malformed solution: %s" s ;
;;
