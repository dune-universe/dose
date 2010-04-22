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

(** Apt command line parsing *)

open ExtLib
open Format822

let space_re = Str.regexp "[ \t]+" 

(* parse the output of "dpkg -l" *)
let parse_inst ch =
  let h = Hashtbl.create 1000 in
  try
    while true do
      let s = (input_line ch) in
      match Str.split space_re s with
      |status::name::version::_ when status = "ii"-> Hashtbl.add h (name,version) ()
      |_ -> ()
    done ;
    h
  with End_of_file -> h

let parse_inst_from_cmd cmd =
  let ch = Unix.open_process_in cmd in
  let h = parse_inst ch in
  let _ = close_in ch in
  h

let parse_inst_from_file file =
  let ch = open_in file in
  let h = parse_inst ch in
  let _ = close_in ch in
  h

(**********************************************************************)

(* parse the a popcon file *)
let parse_popcon s =
  match Str.split space_re s with
  |rank::name::inst::_ -> (int_of_string rank,name,int_of_string inst)
  |_ -> (Printf.eprintf "Parse error %s\n" s ; exit (-1))

(*****************************************************)

(*
1. apt-get install PACKAGE ...
2. apt-get install PACKAGE=VERSION ...
3. apt-get install PACKAGE/RELEASE ...
4. apt-get remove PACKAGE ...
5. apt-get upgrade ...
6. apt-get dist-upgrade ...

"-t RELEASE" option 
*)

type apt_req_only = [ `Pkg of string ]
type apt_req_pkg = [
  | apt_req_only
  |`PkgVer of (string * string)
  |`PkgDst of (string * string)
]

type apt_req =
  |Install of apt_req_pkg list
  |Remove of apt_req_only list
  |Upgrade of string option
  |DistUpgrade of string option

let parse_pkg_only s = `Pkg(parse_package s)

let distro_re = Str.regexp "^\\([^=/]*\\)*/[ \t]*\\(.*\\)$"
let version_re = Str.regexp "^\\([^=]*\\)*=[ \t]*\\(.*\\)$"
let parse_pkg_req suite s =
  try 
    if Str.string_match distro_re s 0 then
      `PkgDst(
        parse_package(Str.matched_group 1 s),
        Str.matched_group 2 s
      )
    else if Str.string_match version_re s 0 then
      `PkgVer(
        parse_package(Str.matched_group 1 s),
        (* Packages.parse_version *)(Str.matched_group 2 s)
      )
    else begin match suite with
    |None -> parse_pkg_only s
    |Some suite -> `PkgDst(s,suite)
    end
  with Not_found -> failwith (Format.sprintf "Bad apt package in request '%s'@." s)
;;

(** parse a string containing an apt-get command line 
    @return a data structure containing the request *)
(* XXX upgrade with suite <> None == Install PkgDst ... *)
let parse_request_apt s =
  if not (String.exists s "apt-get") then failwith "Not a valid apt-get command" ;
  let s = String.slice ~first:((String.find s "apt-get")) s in
  let suite = ref None in
  let options = [
    ("-t", Arg.String (fun l -> suite := Some(l)), "");
    ("-s", Arg.Unit (fun _ -> ()), "") ] 
  in
  let reqlist = ref [] in
  let anon s = reqlist := s :: !reqlist in
  begin
    begin try Arg.parse_argv ~current:(ref 0) (Array.of_list (Str.split space_re s)) options anon ""
    with Arg.Bad s -> failwith s end ;
    match List.rev !reqlist with
    |"install" :: tl -> Install(List.map (parse_pkg_req !suite) tl)
    |"remove" :: tl -> Remove(List.map parse_pkg_only tl)
    |["upgrade"] -> Upgrade(!suite)
    |["dist-upgrade"] -> DistUpgrade(!suite)
    |_ -> failwith (Format.sprintf "Bad apt request '%s'@." s)
  end
;;
(*****************************************************)

(** for details on the apt_preferences format :
    man apt_preferences *)
module Pref = struct

  type pin_t =
    |Release of (string * string) list
    |Origin of string
    |Version of string 

  type package_t = Package of string | Star

  type pin_priority_t = int

  type apt_preferences = {
    package : package_t;
    pin : pin_t ;
    pin_priority : pin_priority_t
  }

end

let parse_pref_labels s =
  let comma_re = Str.regexp "[ \t]*,[ \t]*" in
  let eq_re = Str.regexp "[ \t]*=[ \t]*" in
  List.map (fun s' ->
    match Str.split eq_re s with
    |[v] when (Str.string_match (Str.regexp "[0-9\\.]+") v 0) -> ("v",v)
    |[v] when (Str.string_match (Str.regexp "[a-zA-Z]+") v 0) -> ("a",v)
    |[l;v] -> (l,v)
    |_ -> assert false
  ) (Str.split comma_re s)

let general_re = Str.regexp "^[ \t]*\\*[ \t]*$"
let parse_pref_package s =
  if Str.string_match general_re s 0 then Pref.Star
  else Pref.Package (parse_package s)

let pin_re = Str.regexp "^\\([A-Za-z]+\\)[ \t]+\\(.*\\)$"
let parse_pin s =
  if Str.string_match pin_re s 0 then
    match Str.matched_group 1 s with
    |"release" -> Pref.Release (parse_pref_labels (Str.matched_group 2 s))
    |"version" -> Pref.Version (Str.matched_group 2 s) 
    |"origin"  -> Pref.Origin (Str.matched_group 2 s)
    |_ -> assert false
  else assert false

let parse_priority s = int_of_string s

let parse_preferences_fields p =
  let parse f field = f (single_line field (List.assoc field p)) in
  try
    let e = {
        Pref.package = parse parse_pref_package "package";
        pin = parse parse_pin "pin";
        pin_priority = parse parse_priority "pin-priority";
      }
    in Some e
  with Not_found -> None

(** parse the apt_preferences file *)
let parse_preferences_in f ch =
  let parse_preferences_rec = parse_822_iter parse_preferences_fields in
  parse_preferences_rec f (start_from_channel ch)

