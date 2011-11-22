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

let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt
let debug fmt = Util.make_debug __FILE__ fmt
let fatal fmt = Util.make_fatal __FILE__ fmt

type source = {
  name : Format822.name;
  version : Format822.version;
  binary : Format822.name list;
  architecture : Format822.architecture list;
  build_depends : Format822.builddepsformula;
  build_depends_indep : Format822.builddepsformula;
  build_conflicts : Format822.builddepslist;
  build_conflicts_indep : Format822.builddepslist;
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

let parse_s = Packages.parse_s
let parse_name = Packages.parse_name
let parse_version = Packages.parse_version
let parse_arch = Packages.lexbuf_wrapper Packages_parser.archlist_top
let parse_binary s = List.map fst (Packages.parse_vpkglist s) (* hack XXX *)
let parse_builddepslist = Packages.lexbuf_wrapper Packages_parser.builddepslist_top
let parse_builddepsformula = Packages.lexbuf_wrapper Packages_parser.builddepsformula_top

(* Relationships between source and binary packages
 * http://www.debian.org/doc/debian-policy/ch-relationships.html
 * Build-Depends, Build-Depends-Indep, Build-Conflicts, Build-Conflicts-Indep
*)
let parse_package_stanza filter par =
  let p = {
      name = parse_s ~err:"(MISSING NAME)" parse_name "Package" par;
      version = parse_s ~err:"(MISSING VERSION)" parse_version "Version" par;
      architecture = parse_s ~err:"(MISSING ARCH)" parse_arch "Architecture" par;
      binary = parse_s ~opt:[] ~multi:true parse_binary "Binary" par; 
      build_depends = 
        parse_s ~opt:[] ~multi:true parse_builddepsformula "Build-Depends" par; 
      build_depends_indep =
        parse_s ~opt:[] ~multi:true parse_builddepsformula "Build-Depends-Indep" par;
      build_conflicts = 
        parse_s ~opt:[] ~multi:true parse_builddepslist "Build-Conflicts" par;
      build_conflicts_indep = 
        parse_s ~opt:[] ~multi:true parse_builddepslist "Build-Conflicts-Indep" par 
  }
  in
  if Option.is_none filter then Some p
  else if (Option.get filter) p then Some(p) 
  else None
;;

(** parse a debian Sources file from channel *)
let parse_sources_in file ic =
  info "Parsing Sources file %s..." file;
  let stanza_parser = parse_package_stanza None in
  Format822.parse_from_ch (Packages.packages_parser stanza_parser []) ic

(** parse a debian Sources file *)
let input_raw =
  let module Set = Set.Make(struct type t = source let compare = compare end) in
  let module M = Format822.RawInput(Set) in
  M.input_raw parse_sources_in

let sep = ":" ;;

(** transform a list of sources into dummy packages to be then converted to cudf *)
let sources2packages ?(src="src") archs l =
  let archs = "all"::"any"::archs in
  (* as per policy, if the first arch restriction contains a !
   * then we assume that all archs on the lists are bang-ed.
   * cf: http://www.debian.org/doc/debian-policy/ch-relationships.html 7.1 *)
  let select = function
    |(v,(((false,_)::_) as al)) when 
      List.for_all (fun (_,a) -> not(List.mem a archs)) al -> Some v
    |(v,(((true,_)::_) as al)) when 
      List.exists (fun (_,a) -> List.mem a archs) al -> Some v
    |(v,[]) -> Some v
    |_ -> None
  in
  let conflicts l = List.filter_map select l in
  let depends ll = 
    List.filter_map (fun l ->
      match List.filter_map select l with 
      |[] -> None 
      | l -> Some l
    ) ll
  in
  let bins pkg = String.concat "," pkg.binary in
  List.filter_map (fun pkg ->
    let pkgarchs = pkg.architecture in
    if List.exists (fun a -> List.mem a archs) pkgarchs then
      Some (
      { Packages.default_package with
        Packages.name = src ^ sep ^ pkg.name ;
        source = (pkg.name, Some pkg.version);
        version = pkg.version;
        depends = depends (pkg.build_depends_indep @ pkg.build_depends);
        conflicts = conflicts (pkg.build_conflicts_indep @ pkg.build_conflicts);
        architecture = String.concat "," pkg.architecture;
        extras = [("type",src);("binaries",bins pkg)]
      }
      )
    else None
  ) l
