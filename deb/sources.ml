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
  architecture : Format822.architecture list;
  binaries : string list ;
  build_depends : Format822.builddepsformula;
  build_depends_indep : Format822.builddepsformula;
  build_conflicts : Format822.builddepslist;
  build_conflicts_indep : Format822.builddepslist;
}

let default_source = {
  name = "";
  version = "";
  architecture = [];
  binaries = [];
  build_depends = [];
  build_depends_indep = [];
  build_conflicts = [];
  build_conflicts_indep = [];
}

let parse_s = Packages.parse_s
let parse_name = Packages.parse_name
let parse_version = Packages.parse_version
let parse_builddepslist = Packages.lexbuf_wrapper Packages_parser.builddepslist_top
let parse_builddepsformula = Packages.lexbuf_wrapper Packages_parser.builddepsformula_top

let parse_architectures buildarchlist (_loc,s) =
  let matchsource sourcearchlist buildarchlist = 
    List.exists (fun arch ->
      List.exists(fun source ->
        Architecture.src_matches_arch source arch
      ) sourcearchlist
    ) buildarchlist
  in
  let sourcearchlist = Packages.lexbuf_wrapper Packages_parser.archlist_top (_loc,s) in
  if buildarchlist = [] then sourcearchlist
  else if matchsource sourcearchlist buildarchlist then sourcearchlist
  else raise (Packages.IgnorePackage "")
;;

(* Relationships between source and binary packages
 * http://www.debian.org/doc/debian-policy/ch-relationships.html
 * Build-Depends, Build-Depends-Indep, Build-Conflicts, Build-Conflicts-Indep
*)
let parse_package_stanza filter archs par =
  let parse_archs = parse_architectures archs in
  let parse_arch = Packages.lexbuf_wrapper Packages_parser.archlist_top in
  let p () = {
    name = parse_s ~err:"(MISSING NAME)" parse_name "Package" par;
    version = parse_s ~err:"(MISSING VERSION)" parse_version "Version" par;
    architecture = parse_s ~err:"(MISSING ARCH)" parse_archs "Architecture" par;
    binaries = [];
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
  try
    if Option.is_none filter then Some (p ())
    else if (Option.get filter) par then Some(p ()) 
    else None
  with Packages.IgnorePackage s -> begin
    let n = parse_s ~opt:"?" parse_name "Package" par in
    let v = parse_s ~opt:"?" parse_version "Version" par in
    let al = parse_s ~opt:[] parse_arch "Architecture" par in
    debug "Ignoring Source Package (%s,%s,%s) : %s" n v (String.concat "," al) s;
    None
  end
;;

(** parse a debian Sources file from channel *)
let parse_sources_in ?filter ?(archs=[]) fname ic =
  info "Parsing Sources file %s..." fname;
  let stanza_parser = parse_package_stanza filter archs in
  Format822.parse_from_ch (Packages.packages_parser fname stanza_parser []) ic
;;

(** parse a debian Sources file. 
 [~archs] determines which which architectures should be considered while
 parsing the Souces file. if ~arch is [] then all archs are cosidered *)
let input_raw ?filter ?(archs=[]) =
  let module Set = Set.Make(struct type t = source let compare = compare end) in
  let module M = Format822.RawInput(Set) in
  M.input_raw (parse_sources_in ?filter ~archs)
;;

let sep = ":" ;;

(* as per policy, if the first arch restriction contains a !
 * then we assume that all archs on the lists are bang-ed.
 * cf: http://www.debian.org/doc/debian-policy/ch-relationships.html 7.1
 * use the same rule for buildprofile lists
 *
 * given archs, profile and a dependency with an architecture and profile list,
 * decide whether to select or drop that dependency *)
let select hostarch profile dep =
  let matcharch arch = Architecture.src_matches_arch arch hostarch in
  match dep,profile with
  (* positive arch list, negative profiles, no profile selected
   *  -> only select if any of archs is in the arch list *)
  |(v,(((true,_)::_) as al),(((false,_)::_))),None when
    List.exists (fun (_,a) -> matcharch a) al -> Some v
  (* negative arch list, negative profiles, no profile selected
   *  -> only select if none of archs is in the arch list *)
  |(v,(((false,_)::_) as al),(((false,_)::_))),None when
    List.for_all (fun (_,a) -> not(matcharch a)) al -> Some v
  (* positive arch list, positive profiles, some profile selected
   *  -> only select if any of archs is in the arch list
   *     and the selected profile is in the profile list *)
  |(v,(((true,_)::_) as al),(((true,_)::_) as pl)),(Some p) when
    (List.exists (fun (_,a) -> matcharch a) al) &&
    (List.exists (fun (_,a) -> a = p) pl) -> Some v
  (* positive arch list, negative profiles, some profile selected
   *  -> only select if any of archs is in the arch list
   *     and the selected profile is not in the profile list *)
  |(v,(((true,_)::_) as al),(((false,_)::_) as pl)),(Some p) when
    (List.exists (fun (_,a) -> matcharch a) al) &&
    (List.for_all (fun (_,a) -> a <> p) pl) -> Some v
  (* negative arch list, positive profiles, some profile selected
   *  -> only select if none of archs is in the arch list
   *     and the selected profile is in the profile list *)
  |(v,(((false,_)::_) as al),(((true,_)::_) as pl)),(Some p) when
    (List.for_all (fun (_,a) -> not(matcharch a)) al) &&
    (List.exists (fun (_,a) -> a = p) pl) -> Some v
  (* negative arch list, negative profiles, some profile selected
   *  -> only select if none of archs is in the arch list
   *     and the selected profile is not in the profile list *)
  |(v,(((false,_)::_) as al),(((false,_)::_) as pl)),(Some p) when
    (List.for_all (fun (_,a) -> not(matcharch a)) al) &&
    (List.for_all (fun (_,a) -> a <> p) pl) -> Some v
  (* negative arch list, no profiles
   *  -> only select if none of archs is in the arch list *)
  |(v,(((false,_)::_) as al),[]),_ when
    List.for_all (fun (_,a) -> not(matcharch a)) al -> Some v
  (* positive arch list, no profiles
   *  -> only select if any of archs is in the arch list *)
  |(v,(((true,_)::_) as al),[]),_ when
    List.exists (fun (_,a) -> matcharch a) al -> Some v
  (* no arch list, negative profiles, some profile selected
   *  -> only select if the selected profile is not in the profile list *)
  |(v,[],(((false,_)::_) as pl)),(Some p) when
    List.for_all (fun (_,a) -> a <> p) pl -> Some v
  (* no arch list, positive profiles, some profile selected
   *  -> only select if the selected profile is in the profile list *)
  |(v,[],(((true,_)::_) as pl)),(Some p) when
    List.exists (fun (_,a) -> a = p) pl -> Some v
  (* no arch list, false profiles, no profile selected
   *  -> select *)
  |(v,[],(((false,_)::_))),None -> Some v
  (* no arch list, no profiles
   *  -> select *)
  |(v,[],[]),_ -> Some v
  (* any other case
   *  -> drop *)
  |_ -> None
;;

(** transform a list of sources packages into dummy binary packages.
  * This function preserve the order *)
let sources2packages ?(profiles=false) ?(noindep=false) ?(src="src") buildarch hostarch l =
  let conflicts profile l = List.filter_map (select hostarch profile) l in
  let depends profile ll =
    List.filter_map (fun l ->
      match List.filter_map (select hostarch profile) l with
      |[] -> None 
      |l -> Some l
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
  let add_native_ll = List.map add_native_l in

  (* the package name is encodes as src-<profile>:<package name> if
   * a profile is selected, src:<package-name> otherwise *)
  let src2pkg ?(profile=None) srcpkg =
    let prefix = match profile with None -> src | Some s -> src^"-"^s in
    let extras_profile  = match profile with None -> [] | Some s -> [("profile", s)] in
    let depends_indep   = if noindep then [] else add_native_ll srcpkg.build_depends_indep in
    let conflicts_indep = if noindep then [] else add_native_l srcpkg.build_conflicts_indep in
    (* when crossbuilding (host != build), implicitly depend on build-essential
     * and crossbuild-essential-$hostarch. When compiling natively, implicitly
     * depend on build-essential *)
    let build_essential = if buildarch<>hostarch then
      [[(("build-essential", Some buildarch), None)];[(("crossbuild-essential-"^hostarch, Some buildarch), None)]]
    else
      [[(("build-essential", Some buildarch), None)]]
    in
    { Packages.default_package with
      Packages.name = prefix ^ sep ^ srcpkg.name ;
      source = (srcpkg.name, Some srcpkg.version);
      version = srcpkg.version;
      depends = build_essential @ (depends profile (depends_indep @ srcpkg.build_depends));
      conflicts = conflicts profile (conflicts_indep @ srcpkg.build_conflicts);
      architecture = String.concat "," srcpkg.architecture;
      extras = extras_profile @ [("Type",src)]
    }
  in

  (* search in Build-Depends and Build-Conflicts for buildprofiles
     searching in Build-Depends-Indep and Build-Conflicts-Indep doesnt make sense *)
  let getprofiles pkg =
    let deps = pkg.build_conflicts @ (List.flatten pkg.build_depends) in
    (* XXX certainly we could do better here ... *)
    List.unique (List.map snd (List.fold_left (fun l (_,_,pl) -> pl @ l) [] deps))
  in

  (* right fold to not inverse the order *)
  (* if a profile is selected we add an encoding of the package for each profile *)
  List.fold_right (fun srcpkg al ->
    let pkg = src2pkg srcpkg in
    if profiles then
      pkg :: (List.map (fun p -> src2pkg ~profile:(Some p) srcpkg) (getprofiles srcpkg)) @ al
    else
      pkg::al
  ) l []
;;

(** Check if a package is of "Type" source as encoded by the function sources2packages *)
let is_source ?(src="src") pkg = List.mem ("Type", src) pkg.Packages.extras;;

exception MismatchSrc of Cudf.package list
exception NotfoundSrc

(**
 [get_src_package universe binpkg] returns the source package associate
 with the given binary package.
 
 precondition : the package has "type" bin and the universe contains
 packages of "type" src encoded with sources2packages.
 
 Raise MismatchSrc if there exists a source package with the same name
 but with a different version . Raise NotfoundSrc if the univese does not
 contain either a source package associated with the binary package or
 a source package with the same name but different version.
*)
let get_src_package universe binpkg =
  let sn = CudfAdd.encode ("src:"^(CudfAdd.get_property "source" binpkg)) in
  let sv = int_of_string (CudfAdd.get_property "sourceversion" binpkg) in
  try Cudf.lookup_package universe (sn,sv)
  with Not_found -> begin
    let name = CudfAdd.decode sn in
    let number = CudfAdd.get_property "sourcenumber" binpkg in
    match Cudf.lookup_packages universe sn with
    |[] -> begin
        debug "Cannot find source package %s %s associated to the binary package %s"
        name number (CudfAdd.string_of_package binpkg);
        raise NotfoundSrc
    end
    |othersl -> begin
      let othersrcnumbers = 
        List.map (fun othersrc ->
          CudfAdd.get_property "number" othersrc
        ) othersl
      in
      debug "Cannot find source package %s %s associated to the binary package %s"
      name number (CudfAdd.string_of_package binpkg);
      debug "There exist other versions (%s) of the source package %s in the repository"
      (String.concat " , " othersrcnumbers) name;
      raise (MismatchSrc othersl)
    end
  end
;;

(** Returns an hash table that associates source packages (encoded by the
    function sources2packages) to binary packages in the universe. It is
    possible that a source package is not associated with any binary
    packages.
*)
let srcbin_table universe =
  let h = CudfAdd.Cudf_hashtbl.create (Cudf.universe_size universe) in
  let aux binpkg = 
    if CudfAdd.get_property "type" binpkg = "bin" then begin
      try
        let srcpkg = get_src_package universe binpkg in
        try let l = CudfAdd.Cudf_hashtbl.find h srcpkg in l := binpkg::!l
        with Not_found -> CudfAdd.Cudf_hashtbl.add h srcpkg (ref [binpkg])
      with
      |NotfoundSrc -> () (* this binary is not associated to any src *)
      |MismatchSrc sl ->
          (* we add the src to the table, but we do not associate to any
             binary *)
        List.iter (fun srcpkg -> 
          if not(CudfAdd.Cudf_hashtbl.mem h srcpkg) then
            CudfAdd.Cudf_hashtbl.add h srcpkg (ref [])
        ) sl
    end
  in
  Cudf.iter_packages aux universe ;
  h
;;

(** Returns the list of binary packages associated to a package of "Type" source
    encoded by the function sources2packages. The table h associated each source
    with a list of binaries *)
let get_bin_packages h srcpkg =
  try !(CudfAdd.Cudf_hashtbl.find h srcpkg)
  with Not_found -> begin
    let sn = CudfAdd.decode srcpkg.Cudf.package in
    let sv = CudfAdd.get_property "number" srcpkg in
    debug "Source package %s %s not associated with any binary package" sn sv;
    raise Not_found
  end
;;

(** Returns the set of binary packages generated by the packages in srclist.
    The function get_bin_packages gets a source package and returns the set
    of packages associated to it. If the source package is not known, then
    it is ignored. *)
let binset get_bin_packages srclist =
  List.fold_left (fun acc srcpkg ->
    try CudfAdd.Cudf_set.union acc (CudfAdd.to_set (get_bin_packages srcpkg))
    with Not_found -> acc
  ) CudfAdd.Cudf_set.empty srclist
;;
