(**************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
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
open Packages

type release = {
  origin : string;
  label : string;
  suite : string;
  version: string;
  codename : string;
  date: string;
  architecture: string;
  component : string;
  description: string;
  md5sums: (string * string * string) list;
  sha1: (string * string * string) list;
  sha256: (string * string * string) list
}

let default_release = {
    origin = "";
    label = "";
    suite = "";
    version = "";
    codename = "";
    date = "";
    architecture = "";
    component = "";
    description = "";
    md5sums = [];
    sha1 = [];
    sha256 = []
}

let parse_name = parse_package
let parse_version s = parse_version s
let parse_vpkg = parse_constr
let parse_veqpkg = parse_constr
let parse_conj s = parse_vpkglist parse_vpkg s
let parse_cnf s = parse_vpkgformula parse_vpkg s
let parse_prov s = parse_veqpkglist parse_veqpkg s

exception Eof
(** parse a 822 compliant file.
    @return list of Ipr packages.
    @param parse : paragraph parser
    @param f : filter to be applied to each paragraph 
    @param ExtLib.IO.input channel *)
let parse_822_iter parse f ch =
  let progressbar = Util.progress "Debian.Parse.parse_822_iter" in
  let total = 25000 in (* estimate *)
  let i = ref 0 in
  let l = ref [] in
  try
    while true do
      progressbar (incr i ; !i , total) ; 
      match parse_paragraph ch with
      |None -> raise Eof
      |Some par -> 
        begin match parse par with 
        |None -> ()
        |Some e -> l := (f e) :: !l
        end
    done ;
    !l
  with Eof -> !l

let parse_packages_fields par =
  let guard_field field s f = 
    try
      let l = (single_line field (List.assoc field par)) in
      (* it has a status field and we ignore it if not correctly set *)
      if l = s then Some(f) else None 
    with Not_found -> Some(f) (* this package doesn't have a status field *)
  in
  let parse_s f field = f (single_line field (List.assoc field par)) in
  let parse_m f field = f (String.concat " " (List.assoc field par)) in
  let exec () = 
      {
        Ipr.name = parse_s parse_name "package";
        version = parse_s parse_version "version";
        depends = (try parse_m parse_cnf "depends" with Not_found -> []);
        pre_depends = (try parse_m parse_cnf "pre-depends" with Not_found -> []);
        recommends = (try parse_m parse_conj "recommends" with Not_found -> []);
        suggests = (try parse_m parse_conj "suggests" with Not_found -> []);
        enhances = (try parse_m parse_conj "enhances" with Not_found -> []);
        conflicts = (try parse_m parse_conj "conflicts" with Not_found -> []);
        replaces = (try parse_m parse_conj "replaces" with Not_found -> []);
        provides = (try parse_m parse_prov "provides" with Not_found -> []);
      }
  in
  try guard_field "status" "install ok installed" (exec ())
  with Not_found -> None (* this package doesn't either have version or name *)
;;

(** parse a 822 compliant file 
    @return list of Ipr packages.
    @param f : filter to be applied to each paragraph
    @param ExtLib.IO.input channel *)
let parse_packages_in f ch =
  let parse_packages = parse_822_iter parse_packages_fields in
  parse_packages f (start_from_channel ch)
;;

let parse_release_in ch =
  let parse_release_fields par =
    let parse field = 
      try (single_line field (List.assoc field par))
      with Not_found -> ""
    in
    {
      origin = parse "origin";
      label = parse "label";
      suite = parse "suite";
      version = parse "version";
      codename = parse "codename";
      date = parse "date";
      architecture = parse "architecture";
      component = parse "component";
      description = parse "description";
      md5sums = [];
      sha1 = [];
      sha256 = [] 
    }
  in
  match parse_paragraph (start_from_channel ch) with
  |None -> default_release
  |Some par -> parse_release_fields par
;;

(**********************************************************************)

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
  |_ -> (Printf.eprintf "Parse error %s\n" s ; exit 1)

(**********************************************************************)

(** parse a list of debian Packages file 
    @return a list of unique Ipr packages *)
let input_raw files =
  let timer = Util.Timer.create "Debian.Parse.input_raw" in
  Util.Timer.start timer;
  let s =
    List.fold_left (fun acc f ->
      let ch = (Input.open_chan f) in
      let l = parse_packages_in (fun x -> x) ch in
      let _ = IO.close_in ch in
      List.fold_left (fun s x -> Ipr.Set.add x s) acc l
    ) Ipr.Set.empty files
  in
  Util.Timer.stop timer (Ipr.Set.elements s)
;;

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

let parse_pkg_only s = `Pkg(Packages.parse_package s)

let distro_re = Str.regexp "^\\([^/]*\\)*/[ \t]*\\(.*\\)$"
let version_re = Str.regexp "^\\([^=]*\\)*=[ \t]*\\(.*\\)$"
let parse_pkg_req suite s =
  try 
    if Str.string_match distro_re s 0 then
      `PkgDst(
        Packages.parse_package(Str.matched_group 1 s),
        Str.matched_group 2 s
      )
    else if Str.string_match version_re s 0 then
      `PkgVer(
        Packages.parse_package(Str.matched_group 1 s),
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
  let suite = ref None in
  let options = [ ("-t", Arg.String (fun l -> suite := Some(l)), "") ] in
  let reqlist = ref [] in
  let anon s = reqlist := s :: !reqlist in
  begin
    Arg.parse_argv ~current:(ref 0) (Array.of_list (Str.split space_re s)) options anon "" ;
    if List.mem "install" !reqlist then
      Install(List.map (parse_pkg_req !suite) (List.remove !reqlist "install"))
    else if List.mem "remove" !reqlist then
      Remove(List.map parse_pkg_only (List.remove !reqlist "remove"))
    else if List.mem "upgrade" !reqlist then
      Upgrade(!suite)
    else if List.mem "dist-upgrade" !reqlist then
      DistUpgrade(!suite)
    else 
      failwith (Format.sprintf "Bad apt request '%s'@." s)
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

