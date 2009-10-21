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

(** Debian Specific Ipr to Cudf conversion routines *)

open ExtLib
open Common
open Packages

type tables = {
  virtual_table : (string, unit) Hashtbl.t;
  unit_table : (string, unit) Hashtbl.t ;
  versions_table : (string, string list) Hashtbl.t;
  versioned_table : (string, unit) Hashtbl.t
}

let create n = {
  virtual_table = Hashtbl.create n;
  unit_table = Hashtbl.create n;
  versions_table = Hashtbl.create n;
  versioned_table = Hashtbl.create n;
}

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

let escape s =
  let pkgname_re = Str.regexp "^[%a-z0-9][%a-z0-9.+-]+$" in
  if Str.string_match pkgname_re s 0 then s
  else
    let s0 = String.lowercase s in
    if String.length s0 = 1 then
      escape_string (Printf.sprintf "//%s" s0)
    else
      escape_string s0

let rec unescape s =
  let hex_re = Str.regexp "%[0-9a-f][0-9a-f]" in
  let un s =
    let s = Str.matched_string s in
    let hex = String.sub s 1 2 in
    let n = int_of_string ("0x" ^ hex) in
    String.make 1 (Char.chr n)
  in
  Str.global_substitute hex_re un s

let cudfop = function
  |Some(("<<" | "<"),v) -> Some(`Lt,v)
  |Some((">>" | ">"),v) -> Some(`Gt,v)
  |Some("<=",v) -> Some(`Leq,v)
  |Some(">=",v) -> Some(`Geq,v)
  |Some("=",v) -> Some(`Eq,v)
  |None -> None
  |_ -> assert false

let init_versions_table table =
  let add name version =
    try
      let l = Hashtbl.find table name in
      Hashtbl.replace table name (version::l)
    with Not_found -> Hashtbl.add table name [version]
  in
  let conj_iter =
    List.iter (fun (name,sel) ->
      match cudfop sel with
      |None -> ()
      |Some(_,version) -> add name version
    ) 
  in
  let cnf_iter = 
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        match cudfop sel with
        |None -> ()
        |Some(_,version) -> add name version
      ) disjunction
    )
  in
  fun pkg ->
    add pkg.name pkg.version;
    conj_iter pkg.breaks;
    conj_iter pkg.provides;
    conj_iter pkg.conflicts ;
    cnf_iter pkg.depends;
    cnf_iter pkg.pre_depends
;;

let init_virtual_table table pkg =
  let add name =
    if not(Hashtbl.mem table name) then
      Hashtbl.add table name ()
  in
  List.iter (fun (name,_) -> add name) pkg.provides

let init_unit_table table pkg =
  if not(Hashtbl.mem table pkg.name) then
    Hashtbl.add table pkg.name ()

let init_versioned_table table pkg =
  let add name =
    if not(Hashtbl.mem table name) then
      Hashtbl.add table name ()
  in
  let add_iter_cnf =
    List.iter (fun disjunction ->
      List.iter (fun (name,_)-> add name) disjunction
    ) 
  in
  List.iter (fun (name,_) -> add name) pkg.conflicts ;
  add_iter_cnf pkg.pre_depends ;
  add_iter_cnf pkg.depends
;;

let init_tables pkglist =
  let n = 2 * List.length pkglist in
  let tables = create n in 
  let temp_versions_table = Hashtbl.create n in
  let ivt = init_versions_table temp_versions_table in
  let ivrt = init_virtual_table tables.virtual_table in
  let ivdt = init_versioned_table tables.versioned_table in
  let iut = init_unit_table tables.unit_table in

  List.iter (fun pkg -> ivt pkg ; ivrt pkg ; ivdt pkg ; iut pkg) pkglist ;

  Hashtbl.iter (fun k l ->
    Hashtbl.add tables.versions_table k
    (List.unique (List.sort ~cmp:Version.compare l))
  ) temp_versions_table
  ;
  Hashtbl.clear temp_versions_table ;
  tables

(* versions start from 1 *)
let get_version tables (package,version) =
  let l = Hashtbl.find tables.versions_table package in
  let i = fst(List.findi (fun i a -> a = version) l) in
  i + 1

let loadl tables l =
  List.flatten (
    List.map (fun (name,sel) ->
      match cudfop sel with
        |None ->
            if (Hashtbl.mem tables.virtual_table name) &&
            (Hashtbl.mem tables.versioned_table name) then
              [(escape (name^"--virtual"), None);
               (escape name, None)]
            else
              [(escape name, None)]
        |Some(op,v) ->
            [(escape name,Some(op,get_version tables (name,v)))]
    ) l
  )

let loadlc tables name l = (escape name, None)::(loadl tables l)

let loadlp tables l =
  List.map (fun (name,sel) ->
    match cudfop sel with
      |None  ->
          if (Hashtbl.mem tables.unit_table name) || (Hashtbl.mem tables.versioned_table name)
          then (escape (name^"--virtual"),None)
          else (escape name, None)
      |Some(`Eq,v) ->
          if (Hashtbl.mem tables.unit_table name) || (Hashtbl.mem tables.versioned_table name)
          then (escape (name^"--virtual"),Some(`Eq,get_version tables (name,v)))
          else (escape name,Some(`Eq,get_version tables (name,v)))
      |_ -> assert false
  ) l

let loadll tables ll = List.map (loadl tables) ll

(* ========================================= *)

let preamble = [
  ("number",(`String ""));
  ("source",(`String "")) ;
  ("sourceversion",(`String ""))
]

let add_extra extras pkg =
  let number = ("number",`String pkg.version) in
  let (source,sourceversion) =
    let n,v =
      match pkg.source with
      |"",_ -> (pkg.name,pkg.version)
      |n,None -> (n,pkg.version)
      |n,Some v -> (n,v)
    in
    ("source",`String n), ("sourceversion", `String v)
  in
  let l =
    List.filter_map (fun (n,v) ->
      let prop = String.lowercase n in
      try Some (prop,Cudf_types.parse_basetype v (List.assoc prop pkg.extras))
      with Not_found -> None
    ) extras
  in
  [number;source;sourceversion] @ l

let tocudf tables ?(extras=[]) ?(inst=false) pkg =
    { Cudf.package = escape pkg.name ;
      Cudf.version = get_version tables (pkg.name,pkg.version) ;
      Cudf.depends = loadll tables (pkg.pre_depends @ pkg.depends);
      Cudf.conflicts = loadlc tables pkg.name (pkg.breaks @ pkg.conflicts) ;
      Cudf.provides = loadlp tables pkg.provides ;
      Cudf.installed = inst ;
      Cudf.keep = None ;
      Cudf.extra = add_extra extras pkg ;
    }

let lltocudf = loadll
let ltocudf = loadl

let load_universe l =
  let timer = Util.Timer.create "Debian.Debcudf.load_universe" in
  Util.Timer.start timer;
  let tables =  init_tables l in
  let pl = Cudf.load_universe (List.map (tocudf tables) l) in
  Util.Timer.stop timer pl
