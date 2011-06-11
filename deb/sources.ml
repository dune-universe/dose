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

(** Representation of a parsed source description item. all fields are string *)

open ExtLib
open Common
open Format822

type architecture = string
type source = {
  name : name;
  version : version;
  binary : vpkg list;
  architecture : architecture list;
  build_depends : (vpkg * (bool * architecture) list) list list;
  build_depends_indep : (vpkg * (bool * architecture) list) list list;
  build_conflicts : (vpkg * (bool * architecture) list) list;
  build_conflicts_indep : (vpkg * (bool * architecture) list) list;
}

let default_source = {
  name = "";
  version = "";
  architecture = [];
  binary = [];
  build_depends = [];
  build_depends_indep = [];
  build_conflicts = [];
  build_conflicts_indep = [];
}

let parse_s = Format822.parse_s
let parse_name _ s = Format822.parse_package s
let parse_version _ s = Format822.parse_version s
let parse_arch _ s = String.nsplit s " "
let parse_binary _ s = Format822.list_parser ~sep:"," Format822.parse_constr s
let parse_conj _ s = Format822.parse_vpkglist Format822.parse_builddeps s
let parse_cnf _ s = Format822.parse_vpkgformula Format822.parse_builddeps s

(* Relationships between source and binary packages
 * http://www.debian.org/doc/debian-policy/ch-relationships.html
 * Build-Depends, Build-Depends-Indep, Build-Conflicts, Build-Conflicts-Indep
*)
let parse_package_stanza filter par =
  let aux par = 
    let p = {
        name = parse_s ~err:"(MISSING NAME)" parse_name "Package" par;
        version = parse_s ~err:"(MISSING VERSION)" parse_version "Version" par;
        architecture = parse_s ~err:"(MISSING ARCH)" parse_arch "Architecture" par;
        binary = parse_s ~opt:[] ~multi:true parse_binary "Binary" par; 
        build_depends = parse_s ~opt:[] ~multi:true parse_cnf "Build-Depends" par; 
        build_depends_indep =
          parse_s ~opt:[] ~multi:true parse_cnf "Build-Depends-Indep" par;
        build_conflicts = parse_s ~opt:[] ~multi:true parse_conj "Build-Conflicts" par;
        build_conflicts_indep = 
          parse_s ~opt:[] ~multi:true parse_conj "Build-Conflicts-Indep" par 
    }
    in
    if Option.is_none filter then Some p
    else if (Option.get filter) p then Some(p) else None
  in Format822.parse_packages_fields aux par
;;

(** parse a debian Sources file from channel *)
let parse_sources_in ch =
  let parse_packages =
    Format822.parse_822_iter (
      parse_package_stanza None
    )
  in
  parse_packages (Format822.start_from_channel ch)

(** parse a debian Sources file *)
let input_raw =
  let module Set = Set.Make(struct type t = source let compare = compare end) in
  let module M = Format822.RawInput(Set) in
  M.input_raw parse_sources_in

(** transform a list of sources into dummy packages to be then converted to cudf *)
let sources2packages arch l =
  (* as per policy, if the first arch restriction contains a !
   * then we assume that all archs on the lists are bang-ed.
   * cf: http://www.debian.org/doc/debian-policy/ch-relationships.html 7.1 *)
  let select = function
    |(v,(((false,_)::_) as al)) when List.for_all (fun (_,a) -> not(a = arch)) al -> Some v
    |(v,(((true,_)::_) as al)) when List.exists (fun (_,a) -> a = arch) al -> Some v
    |(v,[]) -> Some v
    |_ -> None
  in
  let conflicts l = List.filter_map select l in
  let depends ll = List.filter_map (fun l ->
    match List.filter_map select l with [] -> None | l -> Some l
    ) ll
  in
  List.filter_map (fun pkg ->
    let archs = pkg.architecture in
    if List.exists (fun a -> a = "all" || a = "any" || a = arch) archs then (
      Some (
      { Packages.default_package with
        Packages.name = "src:" ^ pkg.name ;
        source = (pkg.name, Some pkg.version);
        version = pkg.version;
        depends = depends (pkg.build_depends_indep @ pkg.build_depends);
        conflicts = conflicts (pkg.build_conflicts_indep @ pkg.build_conflicts);
        architecture = String.concat "," pkg.architecture
      }
      )
    )
    else None
  ) l
