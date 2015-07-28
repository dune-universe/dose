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

(** Representation of a PEF stanza. *)

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let parse_s = Debian.Packages.parse_s
let parse_e = Debian.Packages.parse_e
let parse_name (_,s) = s
let parse_version (_,s) = s
let parse_vpkg = Debian.Packages.parse_vpkg
let parse_vpkgformula = Debian.Packages.parse_vpkgformula
let parse_vpkglist = Debian.Packages.parse_vpkglist

(** strip down version of the debian package format *)
class package par = object
  val name : Packages_types.name =
    parse_s ~err:"(MISSING NAME)" parse_name "package" par
  val version : Packages_types.version =
    parse_s ~err:"(MISSING VERSION)" parse_version "version" par
  val depends : Packages_types.vpkgformula =
    parse_s ~opt:[] ~multi:true parse_vpkgformula "depends" par
  val conflicts : Packages_types.vpkglist =
    parse_s ~opt:[] ~multi:true parse_vpkglist "conflicts" par
  val provides : Packages_types.vpkglist =
    parse_s ~opt:[] ~multi:true parse_vpkglist "provides" par

  method name = name
  method version = version
  method depends = depends
  method conflicts = conflicts
  method provides = provides
end

class package_extended ?(extras=[]) par = object
  inherit package par

  val recommends : Packages_types.vpkgformula =
    parse_s ~opt:[] ~multi:true parse_vpkgformula "recommends" par
  val extras : (string * string) list =
    parse_e extras par
  
  method recommends = recommends
  method extras = extras
end

let default_package = new package_extended [("name",(Format822.dummy_loc,""));("version",(Format822.dummy_loc,""))]

let parse_package_stanza extras par =
  Some (new package_extended par)

let parse_packages_in ?(extras=[]) fname ic =
  info "Parsing PEF 822 file %s..." fname;
  let stanza_parser = parse_package_stanza extras in
  Format822.parse_from_ch (
    Debian.Packages.packages_parser fname stanza_parser []
  ) ic

(**/**)
module Set = struct
  let pkgcompare p1 p2 = compare (p1#name,p1#version) (p2#name,p2#version)
  include Set.Make(struct 
    type t = package_extended
    let compare (x:t) (y:t) = pkgcompare x y
  end)
end
(**/**)

let input_raw ?(extras=[]) = 
  let module M = Format822.RawInput(Set) in
  M.input_raw (parse_packages_in ~extras)
;;

let input_raw_ch ?(extras=[]) = 
  let module M = Format822.RawInput(Set) in
  M.input_raw_ch (parse_packages_in ~extras)
;;
