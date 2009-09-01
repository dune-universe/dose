(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

open Common
open ExtLib 

type t
external _open_in : string -> t = "rpm_open_hdlist"
external _close_in : t -> unit = "rpm_close_hdlist"
external parse_paragraph : t -> ( string * string ) list option = "rpm_parse_paragraph"
external parse : t -> ( string * string ) list list = "rpm_parse_hdlists"

let dump_raw ppf s par = 
  Format.fprintf ppf "%s\n%s\n@." s
  (String.concat "\n" 
  (List.map (fun (k,v) -> Printf.sprintf "%s: %s" k v ) (List.rev par))
  )

let flags_to_string flag = 
  match (int_of_string flag) mod 16 with
  |2 -> "<"
  |4 -> ">"
  |8 -> "="
  |10 -> "<="
  |12 -> ">="
  |_ -> ""

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
    if not (Str.string_match (Str.regexp "rpmlib(.*)") name_a.(i) 0) then
      begin 
        let constr =
          if i < (Array.length version_a) then 
            let c = flags_to_string flags_a.(i) in
            let v = version_a.(i) in
            if c <> "" && v <> "" then Some(c,v)
            else None
          else None
        in
        let vpkg = (name_a.(i),constr) in
        acc := vpkg :: !acc
      end 
    done
  with Invalid_argument _ -> dump_raw Format.err_formatter "WARNING: ignoring malformed package (list_deps)" par end
  ;
  List.unique !acc
;;

let fileprovide par =
  let basenames_a = split_string "basenames" par in
  let dirindexes_a = split_string "dirindexes" par in
  let dirnames_a = split_string "dirnames" par in
  let acc = ref [] in
  begin try
    for i = 0 to (Array.length dirindexes_a) - 1 do
      let j = int_of_string dirindexes_a.(i) in
      let elem = Printf.sprintf "%s%s" dirnames_a.(j) basenames_a.(i) in
      acc := (elem,None) :: !acc
    done
  with Invalid_argument _ -> dump_raw Format.err_formatter "WARNING: ignoring malformed package (fileprovide)" par end
  ;
  !acc
;;

let provide_list par = 
  let provide = (try list_deps "provide" par with Not_found -> []) in
  (* let fileprovide = fileprovide par in *)
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
  let progressbar = Util.progress "Rpm.Parse.Hdlists.parse_822_iter" in
  let total = 6000 in (* estimate *)
  let i = ref 0 in
  let l = ref [] in
  try
    while true do
      progressbar (incr i ; !i , total) ;
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
  let epoch = List.assoc "epoch" par in
  let version = List.assoc "version" par in
  let release = List.assoc "release" par in
  if epoch <> "0" then Printf.sprintf "%s:%s-%s" epoch version release
  else Printf.sprintf "%s-%s" version release

let dump ppf f =
  let t = _open_in f in
  try 
  while true ; do
    match parse_paragraph t with
      |None -> raise Eof
      |Some par -> dump_raw ppf "" par
  done
  with Eof -> _close_in t 
