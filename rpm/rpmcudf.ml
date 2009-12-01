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

module T = Trie.Make(
  Map.Make( struct type t = char let compare = Pervasives.compare end)
)

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

let add name version =
  try
    let l = Hashtbl.find temp_versions_table name in
    Hashtbl.replace temp_versions_table name (version::l)
  with Not_found -> Hashtbl.add temp_versions_table name [version]

let init_versions_table pkg =
  add pkg.name pkg.version ;
  List.iter (fun (name,sel) ->
    match cudfop sel with
      |None -> ()
      |Some(_,version) -> add name version
  ) (pkg.provides @ pkg.conflicts)
  ;
  List.iter (fun disjunction ->
    List.iter (fun (name,sel) ->
      match cudfop sel with
        |None -> ()
        |Some(_,version) -> add name version
    ) disjunction
  ) (pkg.depends)
;;

let init_tables ?(cmp=compare) pkglist =
  List.iter (init_versions_table) pkglist ;
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

(* ========================================= *)

let loadl_plain l =
  List.map (fun (name,sel) ->
    match cudfop sel with
      |None -> (CudfAdd.encode name, None)
      |Some(op,v) -> (CudfAdd.encode name,Some(op,get_version (name,v)))
  ) l

let loadlp_plain l =
  List.map (fun (name,sel) ->
    match cudfop sel with
      |None  -> (CudfAdd.encode name, None)
      |Some(`Eq,v) -> (CudfAdd.encode name,Some(`Eq,get_version (name,v)))
      |Some(_,v) -> begin
          let cudfop_to = function
            |None -> assert false
            |Some(c,v) -> Printf.sprintf "(%s,%s)" c v
          in
          Printf.eprintf
          "WARNING. Illegal Versioned Provides %s %s. Normalizing to =\n" 
          name (cudfop_to sel) ; 
          (CudfAdd.encode name,Some(`Eq,get_version (name,v)))
      end
  ) l

let loadll_plain ll = List.map loadl_plain ll

(* ========================================= *)


let tocudf ?(inst=false) pkg =
  { Cudf.default_package with
    Cudf.package = CudfAdd.encode pkg.name ;
    Cudf.version = get_version (pkg.name,pkg.version) ;
    Cudf.depends = loadll_plain pkg.depends ;
    Cudf.conflicts = loadl_plain pkg.conflicts ;
    Cudf.provides = loadlp_plain pkg.provides ;
    Cudf.installed = inst 
  }

let load_universe ?(init=true) ?(cmp=compare) l =
  let timer = Util.Timer.create "Rpm.load_universe" in
  Util.Timer.start timer;
  if init then init_tables ~cmp l ;
  let u = Cudf.load_universe (List.map tocudf l) in
  Util.Timer.stop timer u
