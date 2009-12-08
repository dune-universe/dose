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
open ExtString
open Common
open Packages

type tables = {
  units : (string, int Trie.t) Hashtbl.t;
  files : ((string * string), string) Hashtbl.t;
}

let create n = {
  (* all real packages associated with all versions *)
  units = Hashtbl.create (2 * n);
  (* all files associated to a package that are mentioned as a conflict or
   * depends *)
  files = Hashtbl.create (2 * n);
}

let clear tables =
  Hashtbl.clear tables.units;
  Hashtbl.clear tables.files;
;;

let init_tables pkglist =
  let tables = create (List.length pkglist) in

  (* temp_units is the list of all versions associated to a real package or provide *)
  let temp_units = Hashtbl.create (List.length pkglist) in
  (* all avalaible files *)
  let temp_files = Hashtbl.create (10 * (List.length pkglist)) in

  let add_units name version =
    try
      let l = Hashtbl.find temp_units name in
      Hashtbl.replace temp_units name (version::l)
    with Not_found -> Hashtbl.add temp_units name [version] 
  in

  let add_files (n,v) filename =
    (* if String.starts_with "\\/" filename then *)
      if Hashtbl.mem temp_files filename then
        Hashtbl.add tables.files (n,v) filename
  in

  List.iter (fun pkg ->
    List.iter (fun (file,_) ->
      Hashtbl.add temp_files file ()
    ) pkg.Packages.files 
  ) pkglist;

  List.iter (fun pkg ->
    add_units pkg.name pkg.version ;
    List.iter (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None -> add_units name ""
      |Some(_,version) -> add_units name version
    ) pkg.provides
  ) pkglist;

  List.iter (fun pkg ->
    List.iter (fun (name,sel) ->
      add_files (pkg.name,pkg.version) name;
    ) pkg.conflicts
    ;
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        add_files (pkg.name,pkg.version) name;
      ) disjunction
    ) (pkg.depends)
  ) pkglist
  ;

  let initialid = 2 in
  Hashtbl.iter (fun name l ->
    let vl = List.unique (List.sort ~cmp:Version.compare l) in
    let (_,t) = 
      List.fold_left (fun (i,acc) k ->
        (i+1,Trie.add k i acc)
      ) (initialid,Trie.empty) vl 
    in
    Hashtbl.add tables.units name t ;

  ) temp_units;


  Hashtbl.clear temp_units;
  Hashtbl.clear temp_files;

  tables
;;

let get_versions tables (package,prefix) =
  try
    let trie = Hashtbl.find tables.units package in
    let elements t = Trie.fold (fun k v acc -> v::acc) t [] in
    (elements (Trie.restrict prefix trie))
  with Not_found -> []

let get_version tables (package,version) =
  try
    let trie = Hashtbl.find tables.units package in
    Trie.find version trie
  with Not_found -> assert false

(* ========================================= *)

let expand tables ?(op=`Eq) (name,v) =
  match get_versions tables (name,v) with
  |[] -> [CudfAdd.encode name,Some(op,1)]
  |l ->  List.map (fun x -> (CudfAdd.encode name,Some(op,x))) l

let loadl_plain tables l =
  List.flatten (
    List.map (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None -> [(CudfAdd.encode name, None)]
      |Some(op,v) -> expand tables ~op:op (name,v)
    ) l
  )

let loadlp_plain tables l =
  List.flatten (
    List.map (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None  -> [(CudfAdd.encode name, None)]
      (* we normalize everything to equality *)
      |Some(_,v) -> expand tables (name,v)
    ) l
  )

let loadp_files tables (name,version) =
  List.unique (
    List.map (fun n -> (n,None)
    ) (Hashtbl.find_all tables.files (name,version))
  )

let loadll_plain tables ll = List.map (loadl_plain tables) ll

(* ========================================= *)

let tocudf tables ?(inst=false) pkg =
  { Cudf.default_package with
    Cudf.package = CudfAdd.encode pkg.name ;
    Cudf.version = get_version tables (pkg.name,pkg.version) ;
    Cudf.depends = loadll_plain tables pkg.depends ;
    Cudf.conflicts = loadl_plain tables pkg.conflicts ;
    Cudf.provides = (loadp_files tables (pkg.name,pkg.version)) @ (loadlp_plain tables pkg.provides) ;
    Cudf.installed = inst 
  }

let load_universe l =
  let timer = Util.Timer.create "Rpm.load_universe" in
  Util.Timer.start timer;
  let tables =  init_tables l in
  let u = Cudf.load_universe (List.map (tocudf tables) l) in
  clear tables;
  Util.Timer.stop timer u
