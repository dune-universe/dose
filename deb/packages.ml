(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2011 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

(** Representation of a debian package description item. *)

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let parse_multiarch field (_,s) = match s with
  |("None"|"none"|"No"|"no") -> `No
  |("Allowed"|"allowed") -> `Allowed
  |("Foreign"|"foreign") -> `Foreign
  |("Same"|"same") -> `Same
  |_ -> raise (Format822.Type_error (Printf.sprintf "Field %s has a wrong value : %s" field s))

let parse_source field = Pef.Packages.lexbuf_wrapper field Pef.Packages_parser.source_top
let parse_binarylist field = Pef.Packages.lexbuf_wrapper field Pef.Packages_parser.vpkglist_top

class package ?(name=("Package",None)) ?(version=("Version",None)) ?(depends=("Depends",None))
    ?(conflicts=("Conflicts",None)) ?(provides=("Provides",None)) ?(recommends=("Recommends",None)) 
    ?(architecture=("Architecture",None)) ?(multiarch=("Multi-Arch",None)) ?(source=("Source",None))
    ?(essential=("Essential",None)) ?(build_essential=("Build-Essential",None)) 
    ?(extra_source_only=("Extra-Source-Only",None)) ?(priority=("Priority",None)) 
    ?(pre_depends=("Pre-Depends",None)) ?(suggests=("Suggests",None))
    ?(enhances=("Enhances",None)) ?(breaks=("Breaks",None)) ?(replaces=("Replaces",None))
    ?(extras=([],None)) par = object
  
  inherit Pef.Packages.package ~name ~version ~depends ~conflicts ~provides ~recommends ~extras par

  val architecture : (string * Pef.Packages_types.architecture) =
    let f = Pef.Packages.parse_s ~err:"(MISSING ARCH)" Pef.Packages.parse_string in
    Pef.Packages.get_field_value f par architecture

  val multiarch : (string * Pef.Packages_types.multiarch) =
    let f = Pef.Packages.parse_s ~opt:`No parse_multiarch in
    Pef.Packages. get_field_value f par multiarch

  val source : (string * (Pef.Packages_types.name * Pef.Packages_types.version option)) =
    let f = Pef.Packages.parse_s ~opt:("",None) parse_source in
    Pef.Packages.get_field_value f par source

  val essential : (string * bool) =
    let f = Pef.Packages.parse_s ~opt:false Pef.Packages.parse_bool in
    Pef.Packages.get_field_value f par essential

  val build_essential : (string * bool) =
    let f = Pef.Packages.parse_s ~opt:false Pef.Packages.parse_bool in
    Pef.Packages.get_field_value f par build_essential

  val extra_source_only : (string * bool) =
    let f = Pef.Packages.parse_s ~opt:false Pef.Packages.parse_bool in
    Pef.Packages.get_field_value f par extra_source_only

  val priority : (string * string) =
    let f = Pef.Packages.parse_s ~opt:"" Pef.Packages.parse_string in
    Pef.Packages.get_field_value f par priority

  val pre_depends : (string * Pef.Packages_types.vpkgformula) =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkgformula in
    Pef.Packages.get_field_value f par pre_depends

  val suggests : (string * Pef.Packages_types.vpkgformula) =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkgformula in
    Pef.Packages.get_field_value f par suggests

  val enhances : (string * Pef.Packages_types.vpkgformula) =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkgformula in
    Pef.Packages.get_field_value f par enhances

  val breaks : (string * Pef.Packages_types.vpkglist) =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkglist in
    Pef.Packages.get_field_value f par breaks 

  val replaces : (string * Pef.Packages_types.vpkglist) =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkglist in
    Pef.Packages.get_field_value f par replaces

  method architecture = snd architecture
  method multiarch = snd multiarch
  method essential = snd essential
  method build_essential = snd build_essential
  method extra_source_only = snd extra_source_only
  method priority = snd priority
  method source = snd source
  method pre_depends = snd pre_depends
  method suggests = snd suggests
  method enhances = snd enhances
  method breaks = snd breaks
  method replaces = snd replaces

  method set_multiarch v = {< multiarch = (fst multiarch,v) >}
  method set_essential v = {< essential = (fst essential,v) >}

  method pp oc =
    let pp_multiarch oc = function
      |`No -> Printf.fprintf oc "no"
      |`Same -> Printf.fprintf oc "same"
      |`Foreign -> Printf.fprintf oc "foreign"
      |`Allowed -> Printf.fprintf oc "allowed"
    in
    let pp_source oc = function
      |source,None -> Printf.fprintf oc "%s" source
      |source,Some version -> Printf.fprintf oc "%s (%s)" source version
    in
    Pef.Printer.pp_string oc name;
    Pef.Printer.pp_string oc version;
    Pef.Printer.pp_string oc architecture;
    Pef.Printer.pp_function oc pp_multiarch multiarch;
    Pef.Printer.pp_bool oc essential;
    Pef.Printer.pp_bool oc build_essential;
    Pef.Printer.pp_string oc priority;
    Pef.Printer.pp_function oc pp_source source;

    Pef.Printer.pp_vpkglist oc provides;
    Pef.Printer.pp_vpkgformula oc depends;
    Pef.Printer.pp_vpkgformula oc pre_depends;
    Pef.Printer.pp_vpkglist oc conflicts;
    Pef.Printer.pp_vpkgformula oc recommends;
    Pef.Printer.pp_vpkgformula oc suggests;
 
    Printf.fprintf oc "\n";

end

let parse_package_stanza filter archs extras par =
  let p () = 
    let pkg = new package ~extras:(extras,Some [("Type","bin")]) par in
    (* make sure that the "all" arch is always considered *)
    if archs = [] then pkg else
    if List.mem pkg#architecture ("all"::archs) then pkg
    else
      raise (Pef.Packages.IgnorePackage (
        Printf.sprintf
        "architecture: %s is not included in %s"
        pkg#architecture (ExtString.String.join "," ("all"::archs))
        )
      )
  in
  try
    if Option.is_none filter then Some (p ())
    else if (Option.get filter) par then Some(p ()) 
    else None
  with 
  |Pef.Packages.IgnorePackage s -> begin
      let n = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_name "Package" par in
      let v = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_version "Version" par in
      let a = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_string "Architecture" par in
      warning "Ignoring Package (%s,%s,%s) : %s" n v a s; 
      None
    end
  |Pef.Packages.ParseError (f,s) -> begin
      let n = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_name "Package" par in
      let v = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_version "Version" par in
      let a = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_string "Architecture" par in
      let err = Printf.sprintf "Parser Error in Package (%s,%s,%s) : %s" n v a s in
      raise ( Pef.Packages.ParseError (f,err) )
  end

let status_filter par =
  try
    let (_,s) = (Pef.Packages.assoc "Status" par) in
    match String.nsplit s " " with
    |[_;_;"installed"] -> true
    |_ -> false
  with Not_found -> false

let arch_filter archlist par =
  try
    let (_,s) = (Pef.Packages.assoc "Architecture" par) in
    List.mem s archlist
  with Not_found -> false

let parse_packages_in ?filter ?(archs=[]) ?(extras=[]) fname ic =
  info "Parsing Packages file %s..." fname;
  try
    let stanza_parser = parse_package_stanza filter archs extras in
    Format822.parse_from_ch (Pef.Packages.packages_parser fname stanza_parser []) ic
  with Pef.Packages.ParseError (field,errmsg) -> fatal "Filename %s\n %s : %s" fname field errmsg

let id p = (p#name,p#version,p#architecture)
let (>%) p1 p2 = Pervasives.compare (id p1) (id p2)
module Set = struct
  include Set.Make(struct
    type t = package
    let compare x y =
      let c = x >% y in 
      if c = 0 && x#architecture <> "all" then
        debug
        "the input contains two packages with the same name, version and architecture (%s,%s,%s). Only the latter will be considered."
        x#name x#version x#architecture;
      c
  end)
end

let merge status packages =
  if List.length status > 0 then begin
    info "Merging status file";
    let merge_aux p1 p2 =
      if (p1 >% p2) = 0 then begin
        let paux = p1#set_essential (p1#essential || p2#essential) in
        paux#set_extras (List.unique (p1#extras @ p2#extras))
      end else fatal "Something went wrong while merging status+packages"
    in
    let h = Hashtbl.create (List.length status) in
    List.iter (fun p -> Hashtbl.add h (id p) p) status ;
    let ps =
      List.fold_left (fun acc p ->
        try Set.add (merge_aux p (Hashtbl.find h (id p))) acc
        with Not_found -> Set.add p acc
      ) Set.empty (status @ packages)
    in
    Set.elements ps 
  end
  else 
    packages

let installed_re = Re_pcre.regexp "[a-z]+[ \t]+[a-z]+[ \t]+installed"
let is_installed pkg = 
  try Re_pcre.pmatch ~rex:installed_re (pkg#get_extra "Status")
  with Not_found -> false

let is_on_hold pkg =
  try match String.split (pkg#get_extra "Status") " " with
    |"hold",_ -> true
    | _ -> false
  with Not_found -> false

let default_extras = [
  ("Status", None);
  ("Size", None);
  ("Installed-Size", None);
  ("Multi-Arch", None);
  ("Filename", None);
]

(** input_raw [file] : parse a debian Packages file from [file]
    [~archs] determines which which architectures should be considered while
    parsing the Packages file. if ~arch is [] then all archs are cosidered 
*)
let input_raw ?filter ?(archs=[]) ?(extras=[]) =
  let module M = Format822.RawInput(Set) in
  let extras = default_extras @ extras in
  M.input_raw (parse_packages_in ?filter ~archs ~extras)
;;

(** input_raw_ch ch : parse a debian Packages file from channel [ch] *)
let input_raw_ch ?filter ?(archs=[]) ?(extras=[]) =
  let module M = Format822.RawInput(Set) in
  let extras = default_extras @ extras in
  M.input_raw_ch (parse_packages_in ?filter ~archs ~extras)
