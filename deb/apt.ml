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
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

let space_re = Str.regexp "[ \t]+" ;;

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
  |_ -> fatal "Parse error %s\n" s

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

let parse_pkg_only s = `Pkg(Packages.parse_name (Format822.dummy_loc,s))

let distro_re = Str.regexp "^\\([^=/]*\\)*/[ \t]*\\(.*\\)$" ;;
let version_re = Str.regexp "^\\([^=]*\\)*=[ \t]*\\(.*\\)$" ;;
let parse_pkg_req suite s =
  try 
    if Str.string_match distro_re s 0 then
      `PkgDst(
        Packages.parse_name(Format822.dummy_loc,Str.matched_group 1 s),
        Str.matched_group 2 s
      )
    else if Str.string_match version_re s 0 then
      `PkgVer(
        Packages.parse_name (Format822.dummy_loc,Str.matched_group 1 s),
        Packages.parse_version (Format822.dummy_loc,Str.matched_group 2 s)
      )
    else begin match suite with
    |None -> parse_pkg_only s
    |Some suite -> `PkgDst(s,suite)
    end
  with Not_found -> fatal "Bad apt package in request '%s'" s
;;

(** parse a string containing an apt-get command line 
    @return a data structure containing the request *)
(* XXX upgrade with suite <> None == Install PkgDst ... *)
let parse_request_apt s =
  if not (String.exists s "apt-get") then fatal "Not a valid apt-get command" ;
  let s = String.slice ~first:((String.find s "apt-get")) s in
  let suite = ref None in
  (* XXX we parse a lot of options, but we do not handle them ... *)
  let options = [
    ("-t", Arg.String (fun l -> suite := Some(l)), "");
    ("-s", Arg.Unit (fun _ -> ()), "");
    ("-y", Arg.Unit (fun _ -> ()), "");
    ("-v", Arg.Unit (fun _ -> ()), "");
    ("-f", Arg.Unit (fun _ -> ()), "");
    ("--no-install-recommends", Arg.Unit (fun _ -> ()), "");
    ("--install-recommends", Arg.Unit (fun _ -> ()), "");
    ("--no-upgrade", Arg.Unit (fun _ -> ()), "");
    ("--no-remove", Arg.Unit (fun _ -> ()), "");
    ] 
  in
  let reqlist = ref [] in
  let anon s = reqlist := s :: !reqlist in
  begin
    begin try Arg.parse_argv ~current:(ref 0) (Array.of_list (Str.split space_re s)) options anon ""
    with Arg.Bad s -> fatal "%s" s end ;
    match List.rev !reqlist with
    |"install" :: tl -> Install(List.map (parse_pkg_req !suite) tl)
    |"remove" :: tl -> Remove(List.map parse_pkg_only tl)
    |["upgrade"] -> Upgrade(!suite)
    |["dist-upgrade"] -> DistUpgrade(!suite)
    |_ -> fatal "Bad apt request '%s'" s
  end
;;

let parse_request_aptitude s =
  if not (String.exists s "aptitude") then fatal "Not a valid aptitude command" ;
  let s = String.slice ~first:((String.find s "aptitude")) s in
  let suite = ref None in
  (* XXX we parse a lot of options, but we do not handle them ... *)
  let options = [
    ("-t", Arg.String (fun l -> suite := Some(l)), ""); (* default suite *)
    ("-s", Arg.Unit (fun _ -> ()), "");
    ("-y", Arg.Unit (fun _ -> ()), "");
    ("-v", Arg.Unit (fun _ -> ()), "");
    ("--full-resolver", Arg.Unit (fun _ -> ()), "");
    ("--safe-resolver", Arg.Unit (fun _ -> ()), "");
    ("-f", Arg.Unit (fun _ -> ()), ""); (* fix-broken *)
    ("-r", Arg.Unit (fun _ -> ()), ""); (* with-reccomends *)
    ("--with-recommends", Arg.Unit (fun _ -> ()), "");
    ("-R", Arg.Unit (fun _ -> ()), ""); (* without-reccomends *)
    ("--without-recommends", Arg.Unit (fun _ -> ()), "");
    ] 
  in
  let reqlist = ref [] in
  let anon s = reqlist := s :: !reqlist in
  begin
    begin try Arg.parse_argv ~current:(ref 0) (Array.of_list (Str.split space_re s)) options anon ""
    with Arg.Bad s -> fatal "%s" s end ;
    match List.rev !reqlist with
    |"install" :: tl -> Install(List.map (parse_pkg_req !suite) tl)
    |"remove" :: tl -> Remove(List.map parse_pkg_only tl)
    |["upgrade"] | ["safe-upgrade"] | ["dist-upgrade"] -> Upgrade(!suite)
    |["full-upgrade"] -> DistUpgrade(!suite)
    |_ -> fatal "Bad aptitude request '%s'" s
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

let comma_re = Str.regexp "[ \t]*,[ \t]*" ;;
let eq_re = Str.regexp "[ \t]*=[ \t]*" ;;
let di_re = Str.regexp "[0-9\\.]+" ;;
let al_re = Str.regexp "[a-zA-Z]+" ;;

let parse_pref_labels s =
  List.map (fun s' ->
    match Str.split eq_re s with
    |[v] when (Str.string_match di_re v 0) -> ("v",v)
    |[v] when (Str.string_match al_re v 0) -> ("a",v)
    |[l;v] -> (l,v)
    |_ -> fatal "To many commas in label %s" s
  ) (Str.split comma_re s)

let general_re = Str.regexp "^[ \t]*\\*[ \t]*$" ;;
let parse_pref_package (_,s) =
  if Str.string_match general_re s 0 then Pref.Star
  else Pref.Package (Packages.parse_name (Format822.dummy_loc,s))

let pin_re = Str.regexp "^\\([A-Za-z]+\\)[ \t]+\\(.*\\)$" ;;
let parse_pin (_,s) =
  if Str.string_match pin_re s 0 then
    match Str.matched_group 1 s with
    |"release" -> Pref.Release (parse_pref_labels (Str.matched_group 2 s))
    |"version" -> Pref.Version (Str.matched_group 2 s) 
    |"origin"  -> Pref.Origin (Str.matched_group 2 s)
    |s -> fatal "Unkwnon pin type %s" s
  else fatal "Unkwnon pin format %s" s

let parse_preferences_stanza par =
  {
    Pref.package = 
      Packages.parse_s ~err:"(MISSING PACKAGE)"
      parse_pref_package "Package" par;
    pin = Packages.parse_s ~err:"(MISSING PIN)" parse_pin "Pin" par;
    pin_priority = Packages.parse_s ~err:"(MISSING Pin-Priority)"
      Packages.parse_int "Pin-Priority" par;
  }

let rec preferences_parser stanza_parser acc p =
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> acc
  |Some stanza -> 
      let st = stanza_parser stanza in
      preferences_parser stanza_parser (st::acc) p

(** parse the apt_preferences file *)
let parse_preferences_in ic =
  Format822.parse_from_ch (preferences_parser parse_preferences_stanza []) ic
