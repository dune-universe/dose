(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
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

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

class package ?(name=("Package",None)) ?(version=("Version",None)) ?(depends=("Depends",None))
    ?(conflicts=("Conflicts",None)) ?(provides=("Provides",None)) ?(recommends=("Recommends",None)) 
    ?(notavailable=("NotAvailable",None)) ?(build_depends=("Build-Depends",None)) 
    ?(build_depends_indep=("Build-Depends-Indep",None)) par = object
  
  inherit Pef.Packages.package ~name ~version ~depends ~conflicts ~provides ~recommends par

  val notavailable : string list =
    let f = Pef.Packages.parse_s ~opt:[] (Pef.Packages.parse_string_list ~rex:Pef.Packages.comma_regexp) in
    Pef.Packages.get_field_value f par notavailable

  val build_depends : Pef.Packages_types.vpkgformula =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkgformula in
    Pef.Packages.get_field_value f par build_depends

  val build_depends_indep : Pef.Packages_types.vpkgformula =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkgformula in
    Pef.Packages.get_field_value f par build_depends_indep

  method notavailable = notavailable
  method build_depends = build_depends
  method build_depends_indep = build_depends_indep

end

(* a stanza is not considered if the intersection between the
active switch and the not available switches for a package is
empty *)
let parse_package_stanza switches filter par =
  let p () =
    let pkg = new package par in
    if List.exists (fun s -> not(List.mem s pkg#notavailable)) switches then pkg
    else
      raise (Pef.Packages.IgnorePackage (
        Printf.sprintf
        "None of the active switches [%s] are available [%s]" 
        (ExtString.String.join "," switches)
        (ExtString.String.join "," pkg#notavailable)
        )
      )
  in
  try
    if Option.is_none filter then Some (p ())
    else if (Option.get filter) par then Some (p ())
    else None
  with 
  |Pef.Packages.IgnorePackage s -> begin
      let n = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_name "Package" par in
      let v = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_version "Version" par in
      warning "Ignoring Package (%s,%s) : %s" n v s; 
      None
    end
  |Pef.Packages.ParseError (f,s) -> begin
      let n = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_name "Package" par in
      let v = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_version "Version" par in
      let err = Printf.sprintf "Parser Error in Package (%s,%s) : %s" n v s in
      raise ( Pef.Packages.ParseError (f,err) )
  end

let parse_packages_in ?filter switches fname ic =
  info "Parsing Packages file %s..." fname;
  try
    let stanza_parser = parse_package_stanza switches filter in
    Format822.parse_from_ch (Pef.Packages.packages_parser fname stanza_parser []) ic
  with Pef.Packages.ParseError (field,errmsg) -> fatal "Filename %s\n %s : %s" fname field errmsg

module Set = struct
  let pkgcompare p1 p2 = compare (p1#name,p1#version) (p2#name,p2#version)
  include Set.Make(struct
    type t = package
    let compare (x:t) (y:t) = pkgcompare x y
  end)
end

(* XXX switches = empty ==> all switches are available ?? *)
let input_raw ?filter ?(switches=[]) =
  let module M = Format822.RawInput(Set) in
  M.input_raw (parse_packages_in ?filter switches)

let input_raw_ch ?filter ?(switches=[]) =
  let module M = Format822.RawInput(Set) in
  M.input_raw_ch (parse_packages_in ?filter switches)
