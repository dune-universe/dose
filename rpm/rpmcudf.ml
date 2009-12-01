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

type tables = {
  versions : (string, string list) Hashtbl.t;
}

let create n = {
  versions = Hashtbl.create n;
}

let clear tables =
  Hashtbl.clear tables.versions;
;;

let init_versions_table temp pkg =
  let add name version =
    try
      let l = Hashtbl.find temp name in
      Hashtbl.replace temp name (version::l)
    with Not_found -> Hashtbl.add temp name [version]
  in
  add pkg.name pkg.version ;
  List.iter (fun (name,sel) ->
    match CudfAdd.cudfop sel with
      |None -> ()
      |Some(_,version) -> add name version
  ) (pkg.provides @ pkg.conflicts)
  ;
  List.iter (fun disjunction ->
    List.iter (fun (name,sel) ->
      match CudfAdd.cudfop sel with
        |None -> ()
        |Some(_,version) -> add name version
    ) disjunction
  ) (pkg.depends)
;;

let init_tables pkglist =
  let tables = create (List.length pkglist) in
  let temp = Hashtbl.create (List.length pkglist) in
  List.iter (init_versions_table temp) pkglist ;
  Hashtbl.iter (fun k l ->
    Hashtbl.add tables.versions k
    (List.unique (List.sort ~cmp:Version.compare l))
  ) temp ;
  Hashtbl.clear temp;
  tables

(* versions start from 1 *)
let get_version tables (package,version) =
  let l = Hashtbl.find tables.versions package in
  let i = fst(List.findi (fun i a -> a = version) l) in
  i + 1
;;

(* ========================================= *)

let loadl_plain tables l =
  List.map (fun (name,sel) ->
    match CudfAdd.cudfop sel with
      |None -> (CudfAdd.encode name, None)
      |Some(op,v) -> (CudfAdd.encode name,Some(op,get_version tables (name,v)))
  ) l

let loadlp_plain tables l =
  List.map (fun (name,sel) ->
    match CudfAdd.cudfop sel with
      |None  -> (CudfAdd.encode name, None)
      |Some(`Eq,v) -> (CudfAdd.encode name,Some(`Eq,get_version tables (name,v)))
      |Some(_,v) -> begin
          let cudfop_to = function
            |None -> assert false
            |Some(c,v) -> Printf.sprintf "(%s,%s)" c v
          in
          Printf.eprintf
          "WARNING. Illegal Versioned Provides %s %s. Normalizing to =\n" 
          name (cudfop_to sel) ; 
          (CudfAdd.encode name,Some(`Eq,get_version tables (name,v)))
      end
  ) l

let loadll_plain tables ll = List.map (loadl_plain tables) ll

(* ========================================= *)

let tocudf tables ?(inst=false) pkg =
  { Cudf.default_package with
    Cudf.package = CudfAdd.encode pkg.name ;
    Cudf.version = get_version tables (pkg.name,pkg.version) ;
    Cudf.depends = loadll_plain tables pkg.depends ;
    Cudf.conflicts = loadl_plain tables pkg.conflicts ;
    Cudf.provides = loadlp_plain tables pkg.provides ;
    Cudf.installed = inst 
  }

let load_universe l =
  let timer = Util.Timer.create "Rpm.load_universe" in
  Util.Timer.start timer;
  let tables =  init_tables l in
  let u = Cudf.load_universe (List.map (tocudf tables) l) in
  clear tables;
  Util.Timer.stop timer u
