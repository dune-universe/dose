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

(** Synthesis rpm parser *)

open Common
open ExtLib 

let progressbar = Util.Progress.create "Rpm.Parse.Hdlists.parse_822_iter" ;;
Util.Progress.set_total progressbar 7000 (* estimate *) ;;

type t

(* bindings to the librpm *)
external _open_in : string -> t = "rpm_open_hdlist"
external _close_in : t -> unit = "rpm_close_hdlist"
external parse_paragraph : t -> ( string * string ) list option = "rpm_parse_paragraph"
external parse : t -> ( string * string ) list list = "rpm_parse_hdlists"

let decode_flags f =
  match int_of_string f land 15 with
  | 0 -> `ALL
  | 2 -> `Lt
  |10 -> `Leq
  | 8 -> `Eq
  |12 -> `Geq
  | 4 -> `Gt
  |_ -> (Printf.eprintf "Wrong flag %d" ((int_of_string f) land 15) ; exit 1)

let string_of_rel = function
  | `Lt -> "<"
  | `Leq  -> "<="
  | `Eq -> "="
  | `Geq  -> ">="
  | `Gt -> ">"
  | `ALL -> "ALL"

let dump_raw ppf s par = 
  Format.fprintf ppf "%s\n%s\n@." s
  (String.concat "\n" 
  (List.map (function
    |(("requireflags"|"conflictflags"|"provideflags") as k,v)  ->
        Printf.sprintf "%s: %s" k (
          String.concat ","
          (List.map (fun f -> string_of_rel(decode_flags f)) (Str.split_delim (Str.regexp ",") v))
        )
    |(k,v) -> Printf.sprintf "%s: %s" k v 
  ) (List.rev par))
  )

(***************)

(* from rpm-dist-check . Jerome is always right *)
(* RPMSENSE_RPMLIB | RPMSENSE_MISSINGOK *)
let requires_to_skip_bitmask = (1 lsl 24) lor (1 lsl 19)

(* Dependencies on rpmlib and "suggests" dependencies are skipped *)
let skipped_dep name flags i =
  (int_of_string flags.(i)) land requires_to_skip_bitmask <> 0 ||
  let nm = name.(i) in
  (String.length nm > 8 &&
   nm.[0] = 'r' && nm.[1] = 'p' && nm.[2] = 'm' && nm.[3] = 'l' &&
   nm.[4] = 'i' && nm.[5] = 'b' && nm.[6] = '(')

(***************)

let split_string p par =
  Array.of_list (Str.split_delim (Str.regexp ",") (List.assoc p par))

let list_deps p par =
  let name_s = p ^ "name" in
  let version_s = p ^ "version" in
  let flags_s = p ^ "flags" in
  let name_a = split_string name_s par in
  let version_a = split_string version_s par in
  let flags_a = split_string flags_s par in
  let acc = ref [] in
  begin try 
    for i = 0 to (Array.length name_a) - 1 do
      if not (skipped_dep name_a flags_a i) then
      begin 
        if i < (Array.length version_a) then begin
          let n = name_a.(i) in
          let c = decode_flags flags_a.(i) in
          let v = version_a.(i) in
          let vpkg = (n,(c,v)) in
          acc := vpkg :: !acc
        end
      end 
    done
  with Invalid_argument _ -> dump_raw Format.err_formatter "Warning: ignoring malformed package (list_deps)" par end
  ;
  List.unique !acc
;;

let is_directory (mode: string) =
  try (int_of_string mode) land 0o40000 == 0o40000
  with Failure _ -> false
;;

let fileprovide par =
  let basenames_a = split_string "basenames" par in
  let dirindexes_a = split_string "dirindexes" par in
  let dirnames_a = split_string "dirnames" par in
  let filemodes_a = split_string "filemodes" par in
  let acc = ref [] in
  begin try
    for i = 0 to (Array.length dirindexes_a) - 1 do
      let j = int_of_string dirindexes_a.(i) in
      let elem = Printf.sprintf "%s%s" dirnames_a.(j) basenames_a.(i) in
      acc := ((elem,(`ALL,"")),is_directory filemodes_a.(i)) :: !acc
    done
  with Invalid_argument _ -> dump_raw Format.err_formatter "Warning: ignoring malformed package (fileprovide)" par end
  ;
  !acc
;;

let provide_list par = 
  let provide = (try list_deps "provide" par with Not_found -> []) in
  (List.unique provide)

let depend_list par =
  let l = (try list_deps "require" par with Not_found -> []) in
  List.map (fun e -> [e]) l

exception Eof

let rec parse_822_rec parse f acc t =
  try match parse_paragraph t with
    |None -> raise Eof
    |Some par ->
      begin match parse par with
      |None -> parse_822_rec parse f acc t
      |Some e -> parse_822_rec parse f ((f e)::acc) t
      end
  with Eof -> acc (* no more paragraphs *)

let parse_822_iter parse f ch =
  let l = ref [] in
  try
    while true do
      Util.Progress.progress progressbar ;
      match parse_paragraph ch with
      |None -> raise Eof
      |Some p ->
        begin match parse p with
        |None -> ()
        |Some q -> l := (f q) :: !l
        end
    done ;
    !l
  with Eof -> !l

let parse_name par = List.assoc "package" par

let parse_version par = 
  let epoch = try List.assoc "epoch" par with Not_found -> "0" in
  let version = List.assoc "version" par in
  let release = List.assoc "release" par in
  if epoch <> "0" then Printf.sprintf "%s:%s-%s" epoch version release
  else Printf.sprintf "%s-%s" version release

let dump ppf f =
  let t = _open_in f in
  try 
  while true ; do
    Util.Progress.progress progressbar ;
    match parse_paragraph t with
      |None -> raise Eof
      |Some par -> dump_raw ppf "" par
  done
  with Eof -> _close_in t 
