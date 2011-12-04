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
  fname : string;
  origin : string;
  label : string;
  suite : string;
  version: string;
  codename : string;
  date: string;
  architecture: string;
  component : string;
  notauto: bool;
  autoup: bool;
  description: string;
  md5sums: (string * string * string) list;
  sha1: (string * string * string) list;
  sha256: (string * string * string) list
}

let default_release = {
  fname = "";
  origin = "";
  label = "";
  suite = "";
  version = "";
  codename = "";
  date = "";
  architecture = "";
  component = "";
  notauto = false;
  autoup = false;
  description = "";
  md5sums = [];
  sha1 = [];
  sha256 = []
}

let parse_release_stanza fname par = {
  fname = Filename.basename (fname);
  origin = Packages.parse_s ~opt:"" Packages.parse_string "Origin" par;
  label = Packages.parse_s ~opt:"" Packages.parse_string "Label" par;
  suite = Packages.parse_s ~opt:"" Packages.parse_string "Suite" par;
  version = Packages.parse_s ~opt:"" Packages.parse_string "Version" par;
  codename = Packages.parse_s ~opt:"" Packages.parse_string "Codename" par;
  date = Packages.parse_s ~opt:"" Packages.parse_string "Date" par;
  architecture = Packages.parse_s ~opt:"" Packages.parse_string "Architectures" par;
  component = Packages.parse_s ~opt:"" Packages.parse_string "Components" par;
  notauto = Packages.parse_s ~opt:false Packages.parse_bool "NotAutomatic" par;
  autoup = Packages.parse_s ~opt:false Packages.parse_bool "ButAutomaticUpgrades" par;
  description = Packages.parse_s ~opt:"" Packages.parse_string "Description" par;
  md5sums = [];
  sha1 = [];
  sha256 = []
}

let release_parser stanza_parser fname p =
  match
  Format822_parser.doc_822_sign 
    Format822_lexer.token_822 p.Format822.lexbuf 
  with 
  |Some st -> Some (stanza_parser fname st)
  |None -> None

let parse_release_in fname ic =
  Format822.parse_from_ch (release_parser parse_release_stanza fname) ic
