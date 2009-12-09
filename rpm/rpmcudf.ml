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
  let temp_versions = Hashtbl.create (List.length pkglist) in
  (* all avalaible files *)
  let temp_files = Hashtbl.create (10 * (List.length pkglist)) in

  let add_units name version =
    try
      let l = Hashtbl.find temp_units name in
      Hashtbl.replace temp_units name (version::l)
    with Not_found -> Hashtbl.add temp_units name [version] 
  in

  let add_versions name version =
    try
      let l = Hashtbl.find temp_versions name in
      Hashtbl.replace temp_versions name (version::l)
    with Not_found -> Hashtbl.add temp_versions name [version] 
  in

  let add_files (n,v) filename =
    if Hashtbl.mem temp_files filename then
      Hashtbl.add tables.files (n,v) filename
  in

  List.iter (fun pkg ->
    add_units pkg.name pkg.version ;

    List.iter (fun (file,_) ->
      Hashtbl.add temp_files file ()
    ) pkg.Packages.files ;

    List.iter (fun (name,_) ->
      add_units name pkg.version
    ) pkg.provides
  ) pkglist;

  List.iter (fun pkg ->
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
    ) pkg.depends
  ) pkglist
  ;

  let initialid = 2 in
  Hashtbl.iter (fun name l ->
    let l1 = try Hashtbl.find temp_versions name with Not_found -> [] in
    let vl = List.unique (List.sort ~cmp:Version.compare (l@l1)) in
    let (_,m) = 
      List.fold_left (fun (i,acc) k ->
        (i+1,Trie.add k i acc)
      ) (initialid,Trie.empty) vl 
    in
    Hashtbl.add tables.units name m ;
  ) temp_units;

  Hashtbl.clear temp_units;
  Hashtbl.clear temp_files;
  Hashtbl.clear temp_versions;

  tables
;;

let get_versions tables (package,prefix) =
  if Hashtbl.mem tables.units package then begin
    let all = Hashtbl.find tables.units package in
    try
      let t = Trie.restrict prefix all in
      Trie.fold (fun k v acc -> v::acc) t [] 
    with Not_found -> (
      Util.print_warning "prefix %s does not match any version for unit %s"
      prefix package;
      [1] )
  end else (
    Util.print_warning "unit %s is not mentioned as a provide or real package" package;
    [1]
  )

let get_version tables (package,version) =
  if Hashtbl.mem tables.units package then begin
    let m = Hashtbl.find tables.units package in
    try Trie.find version m
    with Not_found -> assert false
  end else (
    Util.print_warning "!!!!unit %s is not mentioned as a provide or real package" package;
    assert false
  )

(* ========================================= *)

let expand tables ?(op=`Eq) (name,v) =
  List.map (fun x -> (CudfAdd.encode name,Some(op,x)))
  (get_versions tables (name,v))

let loadl_plain tables l =
  List.flatten (
    List.map (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None -> [(CudfAdd.encode name, None)]
      |Some(`Eq,v) -> expand tables (name,v)
      |Some(op,v) -> [(CudfAdd.encode name, Some(op,get_version tables (name,v)))]
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
