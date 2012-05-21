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

(** Eclipse Specific Cudf conversion routines *)

open ExtLib
open Common
open Packages

type tables = {
  versions_table : (string, string list) Hashtbl.t;
  reverse_table : ((string * int), string) Hashtbl.t
}

let create n = {
  versions_table = Hashtbl.create n;
  reverse_table = Hashtbl.create n;
}

type lookup = {
  from_cudf : Cudf.package -> (string * string);
  to_cudf : (string * string) -> Cudf.package
}

let clear tables =
  Hashtbl.clear tables.versions_table;
  Hashtbl.clear tables.reverse_table
;;

let init_versions_table table =
  let add name version =
    try
      let l = Hashtbl.find table name in
      Hashtbl.replace table name (version::l)
    with Not_found -> Hashtbl.add table name [version]
  in
  let conj_iter =
    List.iter (fun ((name,_),sel) ->
      match CudfAdd.cudfop sel with
      |None -> ()
      |Some(_,version) -> add name version
    ) 
  in
  let cnf_iter = 
    List.iter (fun disjunction ->
      List.iter (fun ((name,_),sel) ->
        match CudfAdd.cudfop sel with
        |None -> ()
        |Some(_,version) -> add name version
      ) disjunction
    )
  in
  fun pkg ->
    add pkg.name pkg.version;
    conj_iter pkg.provides;
    conj_iter pkg.conflicts ;
    cnf_iter pkg.depends;
    cnf_iter pkg.recommends;
    conj_iter pkg.suggests
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
  add_iter_cnf pkg.depends
;;

let init_tables ?(compare=Version.compare) pkglist =
  let n = 2 * List.length pkglist in
  let tables = create n in 
  let temp_versions_table = Hashtbl.create n in
  let ivt = init_versions_table temp_versions_table in

  List.iter (fun pkg -> ivt pkg) pkglist ;

  (* XXX I guess this could be a bit faster if implemented with Sets *)
  Hashtbl.iter (fun k l ->
    Hashtbl.add tables.versions_table k
    (List.unique (List.sort ~cmp:compare l))
  ) temp_versions_table
  ;
  Hashtbl.clear temp_versions_table ;
  tables

(* versions start from 1 *)
let get_cudf_version tables (package,version) =
  try
    let l = Hashtbl.find tables.versions_table package in
    let i = fst(List.findi (fun i a -> a = version) l) in
    Hashtbl.replace tables.reverse_table (CudfAdd.encode package,i+1) version;
    i+1
  with Not_found -> assert false

let get_real_version tables (p,i) =
  try Hashtbl.find tables.reverse_table (p,i)
  with Not_found -> ((Printf.eprintf "%s - %d\n" p i) ; assert false)

let loadl tables l =
  List.flatten (
    List.map (fun ((name,_),sel) ->
      match CudfAdd.cudfop sel with
      |None -> [(CudfAdd.encode name, None)]
      |Some(op,v) -> [(CudfAdd.encode name,Some(op,get_cudf_version tables (name,v)))]
    ) l
  )

let loadlc tables name l = (loadl tables l)

let loadlp tables l =
  List.map (fun ((name,_),sel) ->
    match CudfAdd.cudfop sel with
    |None  -> (CudfAdd.encode name, None)
    |Some(`Eq,v) -> (CudfAdd.encode name,Some(`Eq,get_cudf_version tables (name,v)))
    |_ -> assert false
  ) l

let loadll tables ll = List.map (loadl tables) ll

(* ========================================= *)

type extramap = (string * (string * Cudf_types.typedecl1)) list

let preamble = 
  (* number is a mandatory property -- no default *)
  let l = [
    ("suggests",(`Vpkglist (Some [])));
    ("recommends",(`Vpkgformula (Some [])));
    ("number",(`String None)) ]
  in
  CudfAdd.add_properties Cudf.default_preamble l

let add_extra extras tables pkg =
  let number = ("number",`String pkg.version) in
  let l =
    List.filter_map (fun (debprop, (cudfprop,v)) ->
      let debprop = String.lowercase debprop in
      let cudfprop = String.lowercase cudfprop in
      try 
        let s = List.assoc debprop pkg.extras in
        let typ = Cudf_types.type_of_typedecl v in
        Some (cudfprop, Cudf_types_pp.parse_value typ s)
      with Not_found -> None
    ) extras
  in
  let recommends = ("recommends", `Vpkgformula (loadll tables pkg.recommends)) in
  let suggests = ("suggests", `Vpkglist (loadl tables pkg.suggests)) in

  List.filter_map (function
    |(_,`Vpkglist []) -> None
    |(_,`Vpkgformula []) -> None
    |e -> Some e
  )
  [number; recommends ; suggests] @ l
;;

let tocudf tables ?(extras=[]) ?(inst=false) pkg =
    { Cudf.default_package with
      Cudf.package = CudfAdd.encode pkg.name ;
      Cudf.version = get_cudf_version tables (pkg.name,pkg.version) ;
      Cudf.depends = loadll tables pkg.depends;
      Cudf.conflicts = loadlc tables pkg.name pkg.conflicts;
      Cudf.provides = loadlp tables pkg.provides ;
      Cudf.pkg_extra = add_extra extras tables pkg ;
    }

let lltocudf = loadll
let ltocudf = loadl

let load_list l =
  let timer = Util.Timer.create "Eclipse.eclipsecudf.load_list" in
  Util.Timer.start timer;
  let tables = init_tables l in
  let pkglist = List.map (tocudf tables) l in
  clear tables;
  Util.Timer.stop timer pkglist

let load_universe l =
  let pkglist = load_list l in
  let timer = Util.Timer.create "Eclipse.Eclipsecudf.load_universe" in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
