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

(** Representation of a parsed source description item. 
    all fields are string *)

type architecture = string
type source = {
  name : name;
  version : version;
  binary : vpkg list;
  build_depends : (vpkg * (bool * architecture) list) list list;
  build_depends_indep : (vpkg * (bool * architecture) list) list list;
  build_conflicts : (vpkg * (bool * architecture) list) list;
  build_conflicts_indep : (vpkg * (bool * architecture) list) list;
  architecture : architecture list
}

module Set = Set.Make(struct type t = source let compare = compare end)

let parse_name = parse_package
let parse_arch s = Str.split (Str.regexp " ") s
let parse_version s = parse_version s
let parse_binary s = parse_vpkglist parse_constr s
let parse_cnf s = parse_vpkgformula parse_builddeps s
let parse_conj s = parse_vpkglist parse_builddeps s

(* Relationships between source and binary packages
 * http://www.debian.org/doc/debian-policy/ch-relationships.html
 * Build-Depends, Build-Depends-Indep, Build-Conflicts, Build-Conflicts-Indep
*)
let parse_sources_fields par =
  let parse_s f field = f (single_line field (List.assoc field par)) in
  let parse_m f field = f (String.concat " " (List.assoc field par)) in
  let exec () =
      {
        name = parse_s parse_name "package";
        version = parse_s parse_version "version";
        architecture = parse_s parse_arch "architecture";
        binary = (try parse_m parse_binary "binary" with Not_found -> []);
        build_depends = (try parse_m parse_cnf "build-depends" with Not_found -> []);
        build_depends_indep = (try parse_m parse_cnf "build-depends-indep" with Not_found -> []);
        build_conflicts = (try parse_m parse_conj "build-conflicts" with Not_found -> []);
        build_conflicts_indep = (try parse_m parse_conj "build-conflicts-indep" with Not_found -> []);
      }
  in
  try Some (exec ()) with Not_found -> None (* this package doesn't either have version or name *)

let parse_sources_in f ch =
  let parse_packages = parse_822_iter parse_sources_fields in
  parse_packages f (start_from_channel ch)

let input_raw =
  let module M = Format822.RawInput(Set) in
  M.input_raw parse_sources_in
