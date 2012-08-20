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

include Util.Logging(struct let label = __FILE__ end) ;;

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
      binary = []; (* parse_s ~opt:[] ~multi:true parse_binary "Binary" par; *)
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
let parse_sources_in fname ic =
  info "Parsing Sources file %s..." fname;
  let stanza_parser = parse_package_stanza None in
  Format822.parse_from_ch (Packages.packages_parser fname stanza_parser []) ic

(** parse a debian Sources file *)
let input_raw =
  let module Set = Set.Make(struct type t = source let compare = compare end) in
  let module M = Format822.RawInput(Set) in
  M.input_raw parse_sources_in

let sep = ":" ;;

(* as per policy, if the first arch restriction contains a !
 * then we assume that all archs on the lists are bang-ed.
 * cf: http://www.debian.org/doc/debian-policy/ch-relationships.html 7.1
 * use the same rule for buildprofile lists *)
let select archs profile dep = match dep,profile with
  |(v,(((true,_)::_) as al),(((false,_)::_))),None when
    List.exists (fun (_,a) -> List.mem a archs) al -> Some v
  |(v,(((false,_)::_) as al),(((false,_)::_))),None when
    List.for_all (fun (_,a) -> not(List.mem a archs)) al -> Some v
  |(v,(((true,_)::_) as al),(((true,_)::_) as pl)),(Some p) when
    (List.exists (fun (_,a) -> List.mem a archs) al) &&
    (List.exists (fun (_,a) -> a = p) pl) -> Some v
  |(v,(((true,_)::_) as al),(((false,_)::_) as pl)),(Some p) when
    (List.exists (fun (_,a) -> List.mem a archs) al) &&
    (List.for_all (fun (_,a) -> a <> p) pl) -> Some v
  |(v,(((false,_)::_) as al),(((true,_)::_) as pl)),(Some p) when
    (List.for_all (fun (_,a) -> not(List.mem a archs)) al) &&
    (List.exists (fun (_,a) -> a = p) pl) -> Some v
  |(v,(((false,_)::_) as al),(((false,_)::_) as pl)),(Some p) when
    (List.for_all (fun (_,a) -> not(List.mem a archs)) al) &&
    (List.for_all (fun (_,a) -> a <> p) pl) -> Some v
  |(v,(((false,_)::_) as al),[]),_ when
    List.for_all (fun (_,a) -> not(List.mem a archs)) al -> Some v
  |(v,(((true,_)::_) as al),[]),_ when
    List.exists (fun (_,a) -> List.mem a archs) al -> Some v
  |(v,[],(((false,_)::_) as pl)),(Some p) when
    List.for_all (fun (_,a) -> a <> p) pl -> Some v
  |(v,[],(((true,_)::_) as pl)),(Some p) when
    List.exists (fun (_,a) -> a = p) pl -> Some v
  |(v,[],(((false,_)::_))),None -> Some v
  |(v,[],[]),_ -> Some v
  |_ -> None
;;

(** transform a list of sources into dummy packages to be then converted to cudf *)
let sources2packages ?(profiles=false) ?(noindep=false) ?(src="src") archs l =
  let archs = "all"::"any"::archs in
  let conflicts profile l = List.filter_map (select archs profile) l in
  let depends profile ll =
    List.filter_map (fun l ->
      match List.filter_map (select archs profile) l with
      |[] -> None 
      | l -> Some l
    ) ll
  in
  (* In contrast to B-D and B-C, B-D-I and B-C-I requirements must be satisfied
   * by native packages. Despite that, both fields are each concatenated. B-D-I
   * and B-C-I can not contain :any or :native modifiers. Adding :native to
   * B-D-I and B-C-I makes sure they are satisfied by native packages *)
  let add_native_l =
    List.map (function
      |(((name, None), constr), al, pl) ->
          (((name, Some "native"), constr), al, pl)
      |(((name, Some a), constr), al, pl) ->
         warning "modifier %s for indep dependency %s used" a name;
         (((name, Some a), constr), al, pl)
    )
  in
  let add_native_ll = List.map (fun deps -> add_native_l deps) in
  let bins pkg = String.concat "," pkg.binary in

  let src2pkg ?(profile=None) srcpkg =
    let prefix = match profile with None -> src | Some s -> src^"-"^s in
    let extras_profile = match profile with None -> [] | Some s -> [("profile", s)] in
    let depends_indep = if noindep then [] else add_native_ll srcpkg.build_depends_indep in
    let conflicts_indep = if noindep then [] else add_native_l srcpkg.build_conflicts_indep in
    { Packages.default_package with
      Packages.name = prefix ^ sep ^ srcpkg.name ;
      source = (srcpkg.name, Some srcpkg.version);
      version = srcpkg.version;
      depends = [(("build-essential", Some "native"), None)]::(depends profile (depends_indep @ srcpkg.build_depends));
      conflicts = conflicts profile (conflicts_indep @ srcpkg.build_conflicts);
      architecture = String.concat "," srcpkg.architecture;
      extras = extras_profile @ [("type",src);("binaries",bins srcpkg)]
    }
  in

  (* search in Build-Depends and Build-Conflicts for buildprofiles
     searching in Build-Depends-Indep and Build-Conflicts-Indep doesnt make sense *)
  let getprofiles pkg =
    let deps = pkg.build_conflicts @ (List.concat pkg.build_depends) in
    List.unique (List.map (fun (_,p) -> p) (List.fold_left (fun l (_,_,pl) -> pl @ l) [] deps))
  in

  (* right fold to not inverse the order *)
  List.fold_right (fun srcpkg al ->
    let pkgarchs = srcpkg.architecture in
    if List.exists (fun a -> List.mem a archs) pkgarchs then begin
      let pkg = src2pkg srcpkg in
      if profiles then begin
        pkg :: (List.map (fun p -> src2pkg ~profile:(Some p) srcpkg) (getprofiles srcpkg)) @ al
      end else
        pkg::al
    end else al
  ) l []
