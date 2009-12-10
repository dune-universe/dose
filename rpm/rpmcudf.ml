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

  let add_files filename =
    List.iter (fun u -> 
      Hashtbl.add tables.files u filename
    ) (Hashtbl.find_all temp_files filename)
  in

  List.iter (fun pkg ->
    add_units pkg.name pkg.version ;

    List.iter (fun (file,_) ->
      Hashtbl.add temp_files file (pkg.name,pkg.version)
    ) pkg.Packages.files ;

    List.iter (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None -> add_units name ""
      |Some(_,version) -> add_units name version
    ) pkg.provides
  ) pkglist;

  List.iter (fun pkg ->
    List.iter (fun (name,sel) ->
      add_files name;
      match CudfAdd.cudfop sel with
      |None -> ()
      |Some(_,version) -> add_versions name version
    ) pkg.conflicts
    ;
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        add_files name;
        match CudfAdd.cudfop sel with
        |None -> ()
        |Some(_,version) -> add_versions name version
      ) disjunction
    ) pkg.depends
  ) pkglist
  ;

  (* return an list of version list where each sublist contains semantically 
   * equal, but different versions and the outer list is ordered with respect
   * to rpmcmp *)
  let order l =
    let ol = List.unique (List.sort ~cmp:Version.compare l) in
    let rec aux acc l = function
      |[] -> l :: acc
      |h::t when l = [] -> aux acc (h::l) t
      |h::t when Version.compare h (List.hd l) = 0 -> aux acc (h::l) t
      |h::t -> aux (l::acc) [h] t
    in
    List.rev (aux [] [] ol)
  in
  let initialid = 2 in
  Hashtbl.iter (fun name l1 ->
    let l2 = try Hashtbl.find temp_versions name with Not_found -> [] in
    let vl = order (l1@l2) in
    let (_,m) =
      List.fold_left (fun (i,t) l ->
        (i+1,List.fold_left (fun acc k -> Trie.add k i acc) t l)
      ) (initialid,Trie.empty) vl
    in
    Util.print_info "package %s : %s" name 
    (Trie.fold (fun k v acc -> Printf.sprintf "%s (%s = %d)" acc k v) m ""); 
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
      Util.print_warning "prefix %s does not match any version for unit %s" prefix package;
      assert false )
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
    1
  )

(* ========================================= *)

let expand tables (package,version) (name,v) =
  List.filter_map (fun x ->
    if (package,version) = (name,v) then None
    else Some(CudfAdd.encode name,Some(`Eq,x))
  ) (get_versions tables (name,v))

let loadl tables l =
  List.unique (
    List.flatten (
      List.map (fun (name,sel) ->
        match CudfAdd.cudfop sel with
        |None -> [(CudfAdd.encode name, None)]
        |Some(`Eq,v) -> expand tables ("","") (name,v)
        |Some(op,v) -> [(CudfAdd.encode name, Some(op,get_version tables (name,v)))]
      ) l
    )
  )

(* we filter out provides that provide the package itself *)
let loadlp tables (package,version) l =
  List.unique (
    List.flatten (
      List.map (fun (name,sel) ->
        match CudfAdd.cudfop sel with
        |None  -> [(CudfAdd.encode name, None)]
        (* we normalize everything to equality *)
        |Some(`Eq,v) -> expand tables (package,version) (name,v)
        |Some(_,v) -> (
            Util.print_warning "%s selector in provides for package %s" "" package;
            expand tables (package,version) (name,v)
        )
      ) l
    )
  )

let loadp_files tables (name,version) =
  List.unique (
    List.map (fun n -> (n,None)
    ) (Hashtbl.find_all tables.files (name,version))
  )

let loadll tables ll = List.map (loadl tables) ll

(* ========================================= *)

let tocudf tables ?(inst=false) pkg =
  { Cudf.default_package with
    Cudf.package = CudfAdd.encode pkg.name ;
    Cudf.version = get_version tables (pkg.name,pkg.version) ;
    Cudf.depends = loadll tables pkg.depends ;
    Cudf.conflicts = loadl tables pkg.conflicts ;
    Cudf.provides =
      (loadp_files tables (pkg.name,pkg.version)) @ 
      (loadlp tables (pkg.name,pkg.version) pkg.provides) ;
    Cudf.installed = inst 
  }

let load_universe l =
  let timer = Util.Timer.create "Rpm.load_universe" in
  Util.Timer.start timer;
  let tables =  init_tables l in
  let u = Cudf.load_universe (List.map (tocudf tables) l) in
  clear tables;
  Util.Timer.stop timer u
