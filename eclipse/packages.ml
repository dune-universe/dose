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

(** Representation of a eclipse package description item. *)

open ExtLib
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

(** strip down version of the debian package format *)
type package = {
  name : Debian.Format822.name ;
  version : Debian.Format822.version;
  depends : Debian.Format822.vpkgformula;
  conflicts : Debian.Format822.vpkglist;
  provides : Debian.Format822.vpkglist;
  recommends : Debian.Format822.vpkgformula;
  suggests : Debian.Format822.vpkglist;
  extras : (string * string) list;
}

let default_package = {
  name = "";
  version = "";
  depends = [];
  conflicts = [];
  provides = [];
  recommends = [];
  suggests = [];
  extras = [];
}

let parse_s = Debian.Packages.parse_s
let parse_e = Debian.Packages.parse_e
let parse_name (_,s) = s
let parse_version (_,s) = Version.parse_version s
let parse_vpkg = Debian.Packages.parse_vpkg
let parse_vpkgformula = Debian.Packages.parse_vpkgformula
let parse_vpkglist = Debian.Packages.parse_vpkglist

let parse_package_stanza extras par =
  let extras = (* "status":: *) extras in
  Some
    {
      name = parse_s ~err:"(MISSING NAME)" parse_name "package" par;
      version = parse_s ~err:"(MISSING VERSION)" parse_version "version" par;
      depends = parse_s ~opt:[] ~multi:true parse_vpkgformula "depends" par;
      conflicts = parse_s ~opt:[] ~multi:true parse_vpkglist "conflicts" par;
      provides = parse_s ~opt:[] ~multi:true parse_vpkglist "provides" par;
      recommends = parse_s ~opt:[] ~multi:true parse_vpkgformula "recommends" par;
      suggests = parse_s ~opt:[] ~multi:true parse_vpkglist "suggests" par;
      extras = parse_e extras par;
    }

let parse_packages_in ?(extras=[]) fname ic =
  info "Parsing eclipse 822 file %s..." fname;
  let stanza_parser = parse_package_stanza extras in
  Debian.Format822.parse_from_ch (
    Debian.Packages.packages_parser fname stanza_parser []
  ) ic

(**/**)
module Set = struct
  let pkgcompare p1 p2 = compare (p1.name,p1.version) (p2.name,p2.version)
  include Set.Make(struct 
    type t = package
    let compare = pkgcompare
  end)
end
(**/**)

(** input_raw [file] : parse a debian Packages file from [file] *)
let input_raw ?(extras=[]) = 
  let module M = Debian.Format822.RawInput(Set) in
  M.input_raw (parse_packages_in ~extras)
;;

(** input_raw_ch ch : parse a debian Packages file from channel [ch] *)
let input_raw_ch ?(extras=[]) = 
  let module M = Debian.Format822.RawInput(Set) in
  M.input_raw_ch (parse_packages_in ~extras)
;;
