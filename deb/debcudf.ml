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

(** Debian Specific Cudf conversion routines *)

open ExtLib
open Common
open Packages

type tables = {
  virtual_table : (string, unit) Hashtbl.t;
  unit_table : (string, unit) Hashtbl.t ;
  versions_table : (string, int) Hashtbl.t;
  versioned_table : (string, unit) Hashtbl.t;
  reverse_table : (int, string list ref) Hashtbl.t
}

let create n = {
  virtual_table = Hashtbl.create n;
  unit_table = Hashtbl.create n;
  versions_table = Hashtbl.create n;
  versioned_table = Hashtbl.create n;
  reverse_table = Hashtbl.create n;
}

type lookup = {
  from_cudf : Cudf.package -> (string * string);
  to_cudf : (string * string) -> Cudf.package
}

let clear tables =
  Hashtbl.clear tables.virtual_table;
  Hashtbl.clear tables.unit_table;
  Hashtbl.clear tables.versions_table;
  Hashtbl.clear tables.versioned_table;
  Hashtbl.clear tables.reverse_table
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

let init_versions_table t =
  let add name version = 
    if not(Hashtbl.mem t version) then
      Hashtbl.add t version ()
  in
  let conj_iter =
    List.iter (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None -> ()
      |Some(_,version) -> add name version
    ) 
  in
  let cnf_iter = 
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        match CudfAdd.cudfop sel with
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
    conj_iter pkg.replaces;
    cnf_iter pkg.depends;
    cnf_iter pkg.pre_depends;
    cnf_iter pkg.recommends
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
  let l = Hashtbl.fold (fun v _ acc -> v::acc) temp_versions_table [] in
  let add_reverse i v =
     try let l = Hashtbl.find tables.reverse_table i in l := v::!l
     with Not_found -> Hashtbl.add tables.reverse_table i (ref [v])
  in
  let sl = List.sort ~cmp:Version.compare l in
  let rec numbers (prec,i) = function
    |[] -> ()
    |v::t ->
      if Version.equal v prec then begin
        Hashtbl.add tables.versions_table v i;
        add_reverse i v;
        numbers (prec,i) t
      end else begin
        Hashtbl.add tables.versions_table v (i+1);
        add_reverse (i+1) v;
        numbers (v,(i+1)) t
      end
  in
  (* versions start from 1 *)
  numbers ("",1) sl;
  tables
;;

let get_cudf_version tables (package,version) =
  try Hashtbl.find tables.versions_table version
  with Not_found -> assert false

let get_real_version tables (package,cudfversion) =
  try
    match !(Hashtbl.find tables.reverse_table cudfversion) with
    |[] -> assert false
    |[h] -> h
    |l -> List.fold_left min "999999:999999" l
  with Not_found -> assert false

let loadl tables l =
  List.flatten (
    List.map (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None ->
          if (Hashtbl.mem tables.virtual_table name) &&
          (Hashtbl.mem tables.versioned_table name) then
            [(CudfAdd.encode (name^"--virtual"), None);
             (CudfAdd.encode name, None)]
          else
            [(CudfAdd.encode name, None)]
      |Some(op,v) ->
          [(CudfAdd.encode name,Some(op,get_cudf_version tables (name,v)))]
    ) l
  )

(* we add a self conflict here, because in debian each package is in conflict
   with all other versions of the same package *)
let loadlc tables name l = (CudfAdd.encode name, None)::(loadl tables l)

let loadlp tables l =
  List.map (fun (name,sel) ->
    match CudfAdd.cudfop sel with
    |None  ->
        if (Hashtbl.mem tables.unit_table name) || (Hashtbl.mem tables.versioned_table name)
        then (CudfAdd.encode (name^"--virtual"),None)
        else (CudfAdd.encode name, None)
    |Some(`Eq,v) ->
        if (Hashtbl.mem tables.unit_table name) || (Hashtbl.mem tables.versioned_table name)
        then (CudfAdd.encode (name^"--virtual"),Some(`Eq,get_cudf_version tables (name,v)))
        else (CudfAdd.encode name,Some(`Eq,get_cudf_version tables (name,v)))
    |_ -> assert false
  ) l

let loadll tables ll = List.map (loadl tables) ll

(* ========================================= *)

type extramap = (string * (string * Cudf_types.typedecl1)) list

let preamble = 
  (* number is a mandatory property -- no default *)
  let l = [
    ("replaces",(`Vpkglist (Some [])));
    ("recommends",(`Vpkgformula (Some [])));
    ("number",(`String None));
    ("architecture",(`String None));
    ("source",(`String (Some ""))) ;
    ("sourceversion",(`String (Some ""))) ]
  in
  CudfAdd.add_properties Cudf.default_preamble l

let add_extra extras tables pkg =
  let number = ("number",`String pkg.version) in
  let architecture = ("architecture",`String pkg.architecture) in
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
  let replaces = ("replaces", `Vpkglist (loadl tables pkg.replaces)) in
  List.filter_map (function
    |(_,`Vpkglist []) -> None
    |(_,`Vpkgformula []) -> None
    |e -> Some e
  )
  [architecture; number; source; sourceversion; recommends; replaces] @ l
;;

let add_essential = function
  |false -> `Keep_none
  |true -> `Keep_package

let add_inst inst pkg =
  if inst then true 
  else
    try
      match String.nsplit (List.assoc "status" pkg.extras) " " with
      |[_;_;"installed"] -> true
      | _ -> false
    with Not_found -> false

let tocudf tables ?(extras=[]) ?(inst=false) pkg =
    { Cudf.default_package with
      Cudf.package = CudfAdd.encode pkg.name ;
      Cudf.version = get_cudf_version tables (pkg.name,pkg.version) ;
      Cudf.keep = add_essential pkg.essential;
      Cudf.depends = loadll tables (pkg.pre_depends @ pkg.depends);
      Cudf.conflicts = loadlc tables pkg.name (pkg.breaks @ pkg.conflicts) ;
      Cudf.provides = loadlp tables pkg.provides ;
      Cudf.installed = add_inst inst pkg;
      Cudf.pkg_extra = add_extra extras tables pkg ;
    }

let lltocudf = loadll
let ltocudf = loadl

let load_list l =
  let timer = Util.Timer.create "Debian.Debcudf.load_list" in
  Util.Timer.start timer;
  let tables = init_tables l in
  let pkglist = List.map (tocudf tables) l in
  clear tables;
  Util.Timer.stop timer pkglist

let load_universe l =
  let pkglist = load_list l in
  let timer = Util.Timer.create "Debian.Debcudf.load_universe" in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
