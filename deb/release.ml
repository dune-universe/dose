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

(** Representation of a debian release files *) 

open ExtLib

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

let parse field par = try snd(Packages.assoc field par) with Not_found -> ""

let parse_release_stanza par =
  Some {
    origin = parse "Origin" par;
    label = parse "Label" par;
    suite = parse "Suite" par;
    version = parse "Version" par;
    codename = parse "Codename" par;
    date = parse "Date" par;
    architecture = parse "Architectures" par;
    component = parse "Components" par;
    description = parse "Description" par;
    md5sums = [];
    sha1 = [];
    sha256 = []
  }

let parse_packages_in ?filter ?(default_arch=None) ?(extras=[]) ic =
  Format822.parse_from_ch (Packages.packages_parser parse_release_stanza []) ic
