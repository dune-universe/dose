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
open Format822

type name = string
type version = string
type vpkg = string * (string * string) option

(** Representation of a parsed source description item. 
    all fields are string *)
type source = {
  name : name;
  version : version;
  binary : vpkg list;
  build_depends : vpkg list list;
  architecture : string;
}

let parse_name = parse_package
let parse_arch = parse_package
let parse_version s = parse_version s
let parse_cnf s = parse_vpkgformula parse_constr s
let parse_binary s = parse_veqpkglist parse_constr s

let parse_sources_fields par =
  let parse_s f field = f (single_line field (List.assoc field par)) in
  let parse_m f field = f (String.concat " " (List.assoc field par)) in
  let exec () =
      {
        name = parse_s parse_name "package";
        version = parse_s parse_version "version";
        build_depends = (try parse_m parse_cnf "build-depends" with Not_found -> []);
        binary = (try parse_m parse_binary "binary" with Not_found -> []);
        architecture = parse_s parse_arch "architecture";
      }
  in
  try Some (exec ()) with Not_found -> None (* this package doesn't either have version or name *)

let parse_sources_in f ch =
  let parse_packages = parse_822_iter parse_sources_fields in
  parse_packages f (start_from_channel ch)

