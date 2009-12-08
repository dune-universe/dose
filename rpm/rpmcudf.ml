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
  units : (string, string list) Hashtbl.t;
  versions : (string, int Trie.t) Hashtbl.t;
  files : ((string * string), string) Hashtbl.t;
}

let create n = {
  (* all real packages associated with all versions *)
  units = Hashtbl.create (2 * n);
  (* all provides and real packages associated with a trie of versions *)
  versions = Hashtbl.create (2 * n);
  (* all files associated to a package that are mentioned as a conflict or
   * depends *)
  files = Hashtbl.create (2 * n);
}

let clear tables =
  Hashtbl.clear tables.units;
  Hashtbl.clear tables.versions;
  Hashtbl.clear tables.files;
;;

let init_tables pkglist =
  let tables = create (List.length pkglist) in

  (* temp_versions is the list of all versions associated to a package reference *)
  let temp_versions = Hashtbl.create (List.length pkglist) in
  (* temp_units is the list of all versions associated to a real package or
   * provide *)
  let temp_units = Hashtbl.create (List.length pkglist) in
  let temp_files = Hashtbl.create (10 * (List.length pkglist)) in
  let add_versions name version =
    try
      let l = Hashtbl.find temp_versions name in
      Hashtbl.replace temp_versions name (version::l)
    with Not_found -> Hashtbl.add temp_versions name [version] 
  in
  let add_units = Hashtbl.add temp_units in
  let add_files (n,v) filename =
    if String.starts_with "\\/" filename then 
      if Hashtbl.mem temp_files filename then
        Hashtbl.add tables.files (n,v) filename
  in

  (* all avalaible files *)
  List.iter (fun pkg ->
    List.iter (fun (file,_) ->
      Hashtbl.add temp_files file ()
    ) pkg.Packages.files 
  ) pkglist;

  List.iter (fun pkg ->
    add_versions pkg.name pkg.version ;
    add_units pkg.name pkg.version ;

    List.iter (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None -> add_units name ""
      |Some(_,version) -> begin
        add_versions name version;
        add_units name version
      end
    ) pkg.provides
    ;
    List.iter (fun (name,sel) ->
      add_files (pkg.name,pkg.version) name;
      match CudfAdd.cudfop sel with
        |None -> ()
        |Some(_,version) -> add_versions name version
    ) pkg.conflicts
    ;
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        add_files (pkg.name,pkg.version) name;
        match CudfAdd.cudfop sel with
          |None -> ()
          |Some(_,version) -> add_versions name version
      ) disjunction
    ) (pkg.depends)
  ) pkglist
  ;

  Hashtbl.iter (fun name l ->
    let vl = List.unique (List.sort ~cmp:Version.compare l) in
    let (_,t) = 
      List.fold_left (fun (i,acc) k ->
        (i+1,Trie.add k i acc)
      ) (1,Trie.empty) vl 
    in
    Hashtbl.add tables.versions name t ;

    begin match Hashtbl.find_all temp_units name with
    |[] -> ()
    |l -> Hashtbl.add tables.units name (List.unique l) 
    end
  ) temp_versions;


  Hashtbl.clear temp_units;
  Hashtbl.clear temp_versions;
  Hashtbl.clear temp_files;

  tables
;;

(* versions start from 1 *)
let get_version tables (package,version) =
  try
    let t = 
      try Hashtbl.find tables.versions package 
      with Not_found -> assert false
    in (Trie.find version t)
  with Not_found -> assert false

let get_versions tables (package,prefix) =
  try
  let t =
    try Hashtbl.find tables.versions package
    with Not_found -> assert false
  in
  let l = 
    try Hashtbl.find tables.units package 
    with Not_found -> (Printf.eprintf "%s is not in units\n" package ; [])
  in
  let elements t =
    Trie.fold (fun k v acc -> v::acc
      (* if List.mem k l then v::acc else acc *)
    ) t []
  in
(*  let s = Trie.fold (fun k v acc -> k::acc) (Trie.restrict prefix t) [] in
  Printf.printf "name : %s prefix : %s list : %s\n" package prefix
  (String.concat "," s); *)
  (elements (Trie.restrict prefix t))
  with Not_found -> assert false
;;

(* ========================================= *)

let expand tables (name,v) =
  match get_versions tables (name,v) with
  |[] -> None
  |l -> Some(List.map (fun x -> (CudfAdd.encode name,Some(`Eq,x))) l)

let loadl_plain tables l =
  List.flatten (
    List.filter_map (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None -> Some([(CudfAdd.encode name, None)])
      |Some(`Eq,v) -> expand tables (name,v)
      |Some(op,v) -> Some([(CudfAdd.encode name,Some(op,get_version tables (name,v)))])
    ) l
  )

let loadlp_plain tables l =
  List.flatten (
    List.filter_map (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None  -> Some([(CudfAdd.encode name, None)])
      (* we normalize everything to equality *)
      |Some(_,v) -> expand tables (name,v)
    ) l
  )

let loadp_files tables (name,version) =
  List.unique (
    List.map (fun n -> (n,None)
    ) (Hashtbl.find_all tables.files (name,version))
  )

let loadll_plain tables ll =
  List.filter_map (fun l ->
    match loadl_plain tables l with
    |[] -> None
    |l -> Some l
  ) ll

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
