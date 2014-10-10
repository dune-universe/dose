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

open ExtLib
open Common

let pp_source oc = function
  |source,None -> Printf.fprintf oc "%s" source
  |source,Some version -> Printf.fprintf oc "%s (%s)" source version
;;

let pp_multiarch oc = function
  |`None -> Printf.fprintf oc "none"
  |`Same -> Printf.fprintf oc "same"
  |`Foreign -> Printf.fprintf oc "foreign"
  |`Allowed -> Printf.fprintf oc "allowed"
;;

let string_of_vpkg = function
  |((name,None),None) -> name
  |((name,Some arch),None) -> Printf.sprintf "%s:%s" name arch
  |((name,Some arch),Some(op,ver)) -> Printf.sprintf "%s:%s (%s %s)" name arch op ver
  |((name,None),Some(op,ver)) -> Printf.sprintf "%s (%s %s)" name op ver
;;

let pp_vpkg oc vpkg = Printf.fprintf oc "%s" (string_of_vpkg vpkg)

let string_of_vpkglist vpkglist =
  Util.string_of_list ~sep:"," string_of_vpkg vpkglist
;;

let pp_vpkglist oc vpkglist = 
  Printf.fprintf oc "%s" (string_of_vpkglist vpkglist)
;;

let string_of_vpkgformula vpkgformula =
  let string_of_OR = Util.string_of_list ~sep:" |" string_of_vpkg in
  let string_of_AND = Util.string_of_list ~sep:"," string_of_OR in
  string_of_AND vpkgformula
;;

let pp_vpkgformula oc vpkgformula =
  Printf.fprintf oc "%s" (string_of_vpkgformula vpkgformula)
;;

let pp_package oc pkg =
  let open Packages in
  Printf.fprintf oc "Package: %s\n" pkg.name ;
  Printf.fprintf oc "Version: %s\n" pkg.version ;
  Printf.fprintf oc "Architecture: %s\n" pkg.architecture ;
  Printf.fprintf oc "Multi-Arch: %a\n" pp_multiarch pkg.multiarch;
  if pkg.essential then
    Printf.fprintf oc "Essential: yes\n";
  if pkg.priority <> "" then
    Printf.fprintf oc "Priority: %s\n" pkg.priority ;
  if pkg.source <> ("",None) then
    Printf.fprintf oc "Source: %a\n" pp_source pkg.source;

  if List.length pkg.provides > 0 then
    Printf.fprintf oc "Provides: %a\n" pp_vpkglist pkg.provides;
  if List.length pkg.depends > 0 then
    Printf.fprintf oc "Depends: %a\n" pp_vpkgformula pkg.depends;
  if List.length pkg.pre_depends > 0 then
    Printf.fprintf oc "Pre-Depends: %a\n" pp_vpkgformula pkg.pre_depends;
  if List.length pkg.conflicts > 0 then
    Printf.fprintf oc "Conflicts: %a\n" pp_vpkglist pkg.conflicts;
  if List.length pkg.breaks > 0 then
    Printf.fprintf oc "Breaks: %a\n" pp_vpkglist pkg.breaks;
  if List.length pkg.suggests > 0 then
    Printf.fprintf oc "Suggests: %a\n" pp_vpkgformula pkg.suggests;
  if List.length pkg.enhances > 0 then
    Printf.fprintf oc "Enhances: %a\n" pp_vpkgformula pkg.enhances;
  if List.length pkg.recommends > 0 then
    Printf.fprintf oc "Recommends: %a\n" pp_vpkgformula pkg.recommends;
  if List.length pkg.replaces > 0 then
    Printf.fprintf oc "Replaces: %a\n" pp_vpkglist pkg.replaces
;;

let strinf_of_builddep (vpkg,archfilter,buildfilter) =
  let string_of_filter l =
    String.concat " " (
      List.map (fun (b,s) ->
        if b then s else "!"^s
      ) l
    )
  in
  let string_of_bpformula ll =
    String.concat " " (
      List.map (fun l ->
          Printf.sprintf "<%s>" (string_of_filter l)
        ) ll
    )
  in
  match archfilter,buildfilter with
  |[],[] -> string_of_vpkg vpkg
  |_,[] -> Printf.sprintf "%s [%s]" (string_of_vpkg vpkg) (string_of_filter archfilter)
  |[],_ -> Printf.sprintf "%s %s" (string_of_vpkg vpkg) (string_of_bpformula buildfilter)
  |_,_ ->
      Printf.sprintf "%s [%s] %s"
      (string_of_vpkg vpkg) 
      (string_of_filter archfilter)
      (string_of_bpformula buildfilter)
;;

let string_of_builddepformula builddepformula =
  let string_of_OR = Util.string_of_list ~sep:" |" strinf_of_builddep in
  let string_of_AND = Util.string_of_list ~sep:"," string_of_OR in
  string_of_AND builddepformula
;;

let pp_builddepformula oc builddepformula =
    Printf.fprintf oc "%s" (string_of_builddepformula builddepformula)
;;

let string_of_builddeplist builddeplist =
  Util.string_of_list ~sep:"," strinf_of_builddep builddeplist
;;

let pp_builddeplist oc builddeplist =
  Printf.fprintf oc "%s" (string_of_builddeplist builddeplist)
;;

let pp_source oc pkg =
  let open Sources in
  Printf.fprintf oc "Package: %s\n" pkg.name ;
  Printf.fprintf oc "Version: %s\n" pkg.version ;
  Printf.fprintf oc "Architecture: %s\n" (String.concat " " pkg.architecture) ;

  if List.length pkg.binaries > 0 then
    Printf.fprintf oc "Binaries: %s\n" (String.concat ", " pkg.binaries);

  if List.length pkg.build_depends > 0 then
    Printf.fprintf oc "Build-Depends: %a\n" pp_builddepformula pkg.build_depends;
  if List.length pkg.build_conflicts > 0 then
    Printf.fprintf oc "Build-Conflicts: %a\n" pp_builddeplist pkg.build_conflicts;

  if List.length pkg.build_depends_indep > 0 then
    Printf.fprintf oc "Build-Depends-Indeps: %a\n" pp_builddepformula pkg.build_depends_indep;
  if List.length pkg.build_conflicts_indep > 0 then
    Printf.fprintf oc "Build-Conflicts-Indeps: %a\n" pp_builddeplist pkg.build_conflicts_indep;
;;
