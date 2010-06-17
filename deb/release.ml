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
open Format822

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
