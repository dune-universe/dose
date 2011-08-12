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

let parse_release_stanza par =
  {
    origin = Packages.parse_s Packages.parse_string "Origin" par;
    label = Packages.parse_s Packages.parse_string "Label" par;
    suite = Packages.parse_s Packages.parse_string "Suite" par;
    version = Packages.parse_s Packages.parse_string "Version" par;
    codename = Packages.parse_s Packages.parse_string "Codename" par;
    date = Packages.parse_s Packages.parse_string "Date" par;
    architecture = Packages.parse_s Packages.parse_string "Architectures" par;
    component = Packages.parse_s Packages.parse_string "Components" par;
    description = Packages.parse_s Packages.parse_string "Description" par;
    md5sums = [];
    sha1 = [];
    sha256 = []
  }

let rec release_parser stanza_parser p =
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> None
  |Some stanza -> Some(stanza_parser stanza)
  (*
      let st = stanza_parser stanza in
      release_parser stanza_parser (st::acc) p
      *)

let parse_release_in ic =
  Format822.parse_from_ch (release_parser parse_release_stanza) ic
