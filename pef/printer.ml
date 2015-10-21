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

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let string_of_vpkg = function
  |((name,None),None) -> name
  |((name,Some arch),None) -> Printf.sprintf "%s:%s" name arch
  |((name,Some arch),Some(op,ver)) -> Printf.sprintf "%s:%s (%s %s)" name arch op ver
  |((name,None),Some(op,ver)) -> Printf.sprintf "%s (%s %s)" name op ver
;;

let pp_vpkg oc vpkg = Printf.fprintf oc "%s" (string_of_vpkg vpkg)

let string_of_vpkglist (k,vpkglist) =
  if List.length vpkglist > 0 then
    let v = Util.string_of_list ~sep:", " string_of_vpkg vpkglist in
    Printf.sprintf "%s: %s\n" k v
  else ""
;;

let pp_vpkglist oc (k,vpkglist) = 
  Printf.fprintf oc "%s" (string_of_vpkglist (k,vpkglist))
;;

let string_of_vpkgformula (k,vpkgformula) =
  if List.length vpkgformula > 0 then
    let string_of_OR = Util.string_of_list ~sep:" | " string_of_vpkg in
    let string_of_AND = Util.string_of_list ~sep:", " string_of_OR in
    let v = string_of_AND vpkgformula in
    Printf.sprintf "%s: %s\n" k v
  else ""
;;

let pp_vpkgformula oc (k,vpkgformula) =
  Printf.fprintf oc "%s" (string_of_vpkgformula (k,vpkgformula))
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

let string_of_builddepformula (k,builddepformula) =
  if List.length builddepformula > 0 then
    let string_of_OR = Util.string_of_list ~sep:" | " strinf_of_builddep in
    let string_of_AND = Util.string_of_list ~sep:", " string_of_OR in
    let v = string_of_AND builddepformula in
    Printf.sprintf "%s: %s\n" k v
  else ""
;;

let pp_builddepformula oc (k,builddepformula) =
  if List.length builddepformula > 0 then
    Printf.fprintf oc "%s" (string_of_builddepformula (k,builddepformula))
;;

let string_of_builddeplist (k,builddeplist) =
  if List.length builddeplist > 0 then
    let v = Util.string_of_list ~sep:", " strinf_of_builddep builddeplist in
    Printf.sprintf "%s: %s\n" k v
  else ""
;;

let pp_builddeplist oc (k,builddeplist) =
  if List.length builddeplist > 0 then
    Printf.fprintf oc "%s" (string_of_builddeplist (k,builddeplist))
;;

let pp_string oc (k,v) =
  if v <> "" then
    Printf.fprintf oc "%s: %s\n" k v 

let pp_bool oc (k,v) =
  if v then
    Printf.fprintf oc "%s: %b\n" k v

let pp_yes oc (k,v) =
  if v then
    Printf.fprintf oc "%s: %s\n" k (if v then "yes" else "no")

let pp_function oc to_string (k,v) =
  match to_string v with
  |"" -> ()
  |s -> Printf.fprintf oc "%s: %s\n" k s

let pp_string_list ?(sep=", ") oc (k,v) =
  if List.length v > 0 then
    Printf.fprintf oc "%s: %s\n" k (Util.string_of_list ~sep (fun s -> s) v)

let string_of_vpkgreq = function
  | None,vpkg,None -> string_of_vpkg vpkg
  | None,vpkg,Some suite -> Printf.sprintf "%s/%s" (string_of_vpkg vpkg) suite
  | Some Packages_types.I, vpkg, None -> Printf.sprintf "+%s" (string_of_vpkg vpkg)
  | Some Packages_types.R, vpkg, None -> Printf.sprintf "-%s" (string_of_vpkg vpkg)
  | Some Packages_types.I, vpkg, Some suite -> Printf.sprintf "+%s/%s" (string_of_vpkg vpkg) suite
  | Some Packages_types.R, vpkg, Some suite -> Printf.sprintf "-%s/%s" (string_of_vpkg vpkg) suite
