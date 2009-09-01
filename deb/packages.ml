(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

open ExtLib
open Common
open Format822

type package = {
  name : name ;
  version : version;
  source : (name * version option) ;
  depends : vpkg list list;
  pre_depends : vpkg list list;
  recommends : vpkg list;
  suggests : vpkg list;
  enhances : vpkg list;
  conflicts : vpkg list;
  breaks : vpkg list;
  replaces : vpkg list;
  provides : veqpkg list;
}

let default_package = {
  name = "";
  version = "";
  depends = [];
  source = ("",None);
  pre_depends = [];
  recommends = [];
  suggests = [];
  enhances = [];
  conflicts = [];
  breaks = [];
  replaces = [];
  provides = [];
}

module Set = Set.Make(struct type t = package let compare = compare end)

let parse_name = parse_package
let parse_vpkg = parse_constr
let parse_veqpkg = parse_constr
let parse_conj s = parse_vpkglist parse_vpkg s
let parse_cnf s = parse_vpkgformula parse_vpkg s
let parse_prov s = parse_veqpkglist parse_veqpkg s

let parse_packages_fields par =
  let guard_field field s f = 
    try
      let l = (single_line field (List.assoc field par)) in
      (* it has a status field and we ignore it if not correctly set *)
      if l = s then Some(f) else None 
    with Not_found -> Some(f) (* this package doesn't have a status field *)
  in
  let parse_s f field = f (single_line field (List.assoc field par)) in
  let parse_m f field = f (String.concat " " (List.assoc field par)) in
  let exec () = 
      {
        name = parse_s parse_name "package";
        version = parse_s parse_version "version";
        source = (try parse_s parse_source "source" with Not_found -> ("",None));
        depends = (try parse_m parse_cnf "depends" with Not_found -> []);
        pre_depends = (try parse_m parse_cnf "pre-depends" with Not_found -> []);
        recommends = (try parse_m parse_conj "recommends" with Not_found -> []);
        suggests = (try parse_m parse_conj "suggests" with Not_found -> []);
        enhances = (try parse_m parse_conj "enhances" with Not_found -> []);
        conflicts = (try parse_m parse_conj "conflicts" with Not_found -> []);
        breaks = (try parse_m parse_conj "breaks" with Not_found -> []);
        replaces = (try parse_m parse_conj "replaces" with Not_found -> []);
        provides = (try parse_m parse_prov "provides" with Not_found -> []);
      }
  in
  try guard_field "status" "install ok installed" (exec ())
  with Not_found -> None (* this package doesn't either have version or name *)

let parse_packages_in f ch =
  let parse_packages = parse_822_iter parse_packages_fields in
  parse_packages f (start_from_channel ch)

let input_raw = 
  let module M = Format822.RawInput(Set) in
  M.input_raw parse_packages_in
