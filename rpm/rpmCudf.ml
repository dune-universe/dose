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

let cudfop = function
  |Some(("<<" | "<"),v) -> Some(`Lt,v)
  |Some((">>" | ">"),v) -> Some(`Gt,v)
  |Some("<=",v) -> Some(`Leq,v)
  |Some(">=",v) -> Some(`Geq,v)
  |Some("=",v) -> Some(`Eq,v)
  |None -> None
  |_ -> assert false

let versions_table = Hashtbl.create 50000
let temp_versions_table = Hashtbl.create 50000
(* let deps_table = Hashtbl.create 50000 *)

let init_versions_table =
  let add name version =
    try
      let l = Hashtbl.find temp_versions_table name in
      Hashtbl.replace temp_versions_table name (version::l)
    with Not_found -> Hashtbl.add temp_versions_table name [version]
  in
  fun pkg ->
    add pkg.name pkg.version ;
    begin
      List.iter (fun (name,sel) ->
        match cudfop sel with
          |None -> ()
          |Some(_,version) -> add name version
      ) 
      (pkg.provides @ pkg.conflicts)
    end
    ;
    begin
      List.iter (fun disjunction ->
        List.iter (fun (name,sel) ->
          match cudfop sel with
            |None -> ()
            |Some(_,version) -> add name version
        ) disjunction
      )
      (pkg.depends)
    end
;;

let init_tables ?(cmp=compare) pkglist =
  List.iter (fun pkg -> init_versions_table pkg;) pkglist ;
  Hashtbl.iter (fun k l ->
    Hashtbl.add versions_table k
    (List.unique (List.sort ~cmp:cmp l))
  ) temp_versions_table ;
  Hashtbl.clear temp_versions_table

(* versions start from 1 *)
let get_version (package,version) =
  let l = Hashtbl.find versions_table package in
  let i = fst(List.findi (fun i a -> a = version) l) in
  i + 1
;;

let escape_string s =
  let make_hex chr = Printf.sprintf "%%%x" (Char.code chr) in
  let notallowed_re = Str.regexp "[^a-z0-9.+-]" in
  let n = String.length s in
  let b = Buffer.create n in
  for i = 0 to n-1 do
    let s' = String.of_char s.[i] in
    if Str.string_match notallowed_re s' 0 then
      Buffer.add_string b (make_hex s.[i])
    else
      Buffer.add_string b s'
  done;
  Buffer.contents b
;;

let escape s = 
  let pkgname_re = Str.regexp "^[%a-z0-9][%a-z0-9.+-]+$" in
  if Str.string_match pkgname_re s 0 then s
  else 
    let s0 = String.lowercase s in
    if String.length s0 = 1 then
      escape_string (Printf.sprintf "//%s" s0)
    else
      escape_string s0
;;

(* ========================================= *)

let loadl_plain l =
  List.map (fun (name,sel) ->
    match cudfop sel with
      |None -> (escape name, None)
      |Some(op,v) -> (escape name,Some(op,get_version (name,v)))
  ) l

let loadlp_plain l =
  List.map (fun (name,sel) ->
    match cudfop sel with
      |None  -> (escape name, None)
      |Some(`Eq,v) -> (escape name,Some(`Eq,get_version (name,v)))
      |Some(_,v) -> begin
          let cudfop_to = function
            |None -> assert false
            |Some(c,v) -> Printf.sprintf "(%s,%s)" c v
          in
          Printf.eprintf
          "WARNING. Illegal Versioned Provides %s %s. Normalizing to =\n" 
          name (cudfop_to sel) ; 
          (escape name,Some(`Eq,get_version (name,v)))
      end
  ) l

let loadll_plain ll = List.map loadl_plain ll

(* ========================================= *)

let tocudf ?(inst=false) pkg =
  { Cudf.default_package with
    Cudf.package = escape pkg.name ;
    Cudf.version = get_version (pkg.name,pkg.version) ;
    Cudf.depends = loadll_plain pkg.depends ;
    Cudf.conflicts = loadl_plain ( pkg.conflicts @ pkg.obsoletes ) ;
    Cudf.provides = loadlp_plain pkg.provides ;
    Cudf.installed = inst 
  }

let load_universe ?(init=true) ?(cmp=compare) l =
  let timer = Util.Timer.create "Ipr.load_universe" in
  Util.Timer.start timer;
  if init then init_tables ~cmp l ;
  let u = Cudf.load_universe (List.map tocudf l) in
  Util.Timer.stop timer u
