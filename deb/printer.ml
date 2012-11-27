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
open Packages

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
  Util.string_of_list string_of_vpkg " , " vpkglist
;;

let pp_vpkglist oc vpkglist = 
  Printf.fprintf oc "%s" (string_of_vpkglist vpkglist)
;;

let string_of_vpkgformula vpkgformula =
  let string_of_OR = Util.string_of_list string_of_vpkg " | " in
  let string_of_AND = Util.string_of_list string_of_OR " , " in
  string_of_AND vpkgformula
;;

let pp_vpkgformula oc vpkgformula =
  Printf.fprintf oc "%s" (string_of_vpkgformula vpkgformula)
;;

let pp_package oc pkg =
  let name =
    if Sources.is_source pkg then String.slice ~first:4 pkg.name 
    else pkg.name 
  in
  Printf.fprintf oc "Package: %s\n" name ;
  Printf.fprintf oc "Version: %s\n" pkg.version ;
  Printf.fprintf oc "Architecture: %s\n" pkg.architecture ;
  Printf.fprintf oc "Multi-Arch: %a\n" pp_multiarch pkg.multiarch;
  Printf.fprintf oc "Essential: %b\n" pkg.essential ;
  if pkg.priority <> "" then
    Printf.fprintf oc "Priority: %s\n" pkg.priority ;
  if pkg.source <> ("",None) then
    Printf.fprintf oc "Source: %a\n" pp_source pkg.source;

  if List.length pkg.provides > 0 then
    Printf.fprintf oc "Provides: %a\n" pp_vpkglist pkg.provides;
  if List.length pkg.depends > 0 then
    Printf.fprintf oc "Depends: %a\n" pp_vpkgformula pkg.depends;
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
