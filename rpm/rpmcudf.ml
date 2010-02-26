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
  fileconflicts : ((string * string), vpkg)  Hashtbl.t;
}

let create n = {
  (* all real packages associated with all versions *)
  units = Hashtbl.create (2 * n);
  (* all files associated to a package that are mentioned as a conflict or
   * depends *)
  files = Hashtbl.create (2 * n);
  fileconflicts = Hashtbl.create (10 * n);
}

let clear tables =
  Hashtbl.clear tables.units;
  Hashtbl.clear tables.files;
  Hashtbl.clear tables.fileconflicts;
;;

let init_tables pkglist =
  let tables = create (List.length pkglist) in

  (* temp_units is the list of all versions associated to a real package or provide *)
  let temp_units = Hashtbl.create (List.length pkglist) in
  let temp_versions = Hashtbl.create (List.length pkglist) in
  (* all avalaible files *)
  let temp_files = Hashtbl.create (10 * (List.length pkglist)) in

  let add_list t k v =
    let l =
      try Hashtbl.find t k with Not_found ->
      let l = ref [] in Hashtbl.add t k l; l
    in
    if v <> "" then l := v :: !l
  in

  let add_units name version = add_list temp_units name version in
  let add_versions name version = add_list temp_versions name version in

  let add_files filename =
    List.iter (fun u -> 
      Hashtbl.add tables.files u filename
    ) (Hashtbl.find_all temp_files filename)
  in

  List.iter (fun pkg ->
    add_units pkg.name pkg.version ;

    List.iter (fun ((file,_),is_dir) ->
      if not is_dir then
        Hashtbl.add temp_files file (pkg.name,pkg.version)
    ) pkg.Packages.files ;

    List.iter (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None -> add_units name ""
      |Some(_,version) -> add_units name version
    ) pkg.provides
  ) pkglist
  ;

  List.iter (fun pkg ->

    List.iter (fun ((file,_),_) ->
      List.iter (fun (n,v) ->
        if n <> pkg.Packages.name && v <> pkg.Packages.version then
          let (pn,pv) = (pkg.Packages.name,pkg.Packages.version) in
          begin
            (* Printf.printf "adding %s <-> %s to file conflicts table (file: %s)\n" pn n file; *)
            Hashtbl.add tables.fileconflicts (pn,pv) (n,Some("=",v))
          end
      ) (Hashtbl.find_all temp_files file)
    ) pkg.Packages.files ;

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

  let initialid = 2 in
  let order l = List.unique (List.sort ~cmp:Version.compare l) in
  Hashtbl.iter (fun name {contents = l1} ->
    let l2 = try !(Hashtbl.find temp_versions name) with Not_found -> [] in
    let vl = order (l1@l2) in
    let (_,m) =
      List.fold_left (fun (i,t) k ->
        (i+1,Trie.add k i t)
      ) (initialid,Trie.empty) vl
    in
    (*
    Util.print_info "package %s : %s" name 
    (Trie.fold (fun k v acc -> Printf.sprintf "%s (%s = %d)" acc k v) m ""); 
    *)
    Hashtbl.add tables.units name m ;
  ) temp_units;

  Hashtbl.clear temp_units;
  Hashtbl.clear temp_files;
  Hashtbl.clear temp_versions;

  tables
;;

(* ATT: we use version 1 for a version of a non existent package - 
   neither as a real package or a provide 
   strict : match only version with EVR tuples *)
let get_versions tables (package,prefix) =
  if Hashtbl.mem tables.units package then begin
    let all =
      try Hashtbl.find tables.units package
      with Not_found -> assert false
    in
    try
      let t = Trie.restrict prefix all in
      Trie.fold (fun k v acc -> 
        match Version.parse_version k with
        |Some(_,_,Some _) -> v::acc
        |Some(_,_,None) -> acc
        |None -> assert false
      ) t [] 
    with Not_found -> (
      Util.print_warning "prefix %s does not match any version for unit %s" prefix package;
      assert false )
  end else (
    (* Util.print_warning "unit %s is not mentioned as a provide or real package" package; *)
    [1]
  )

let get_version tables (package,version) =
  if Hashtbl.mem tables.units package then begin
    let m = Hashtbl.find tables.units package in
    try Trie.find version m
    with Not_found -> assert false
  end else (
    (* Util.print_warning "unit %s is not mentioned as a provide or real package" package; *)
    1
  )

(* ========================================= *)

let foldl1 f = function
  | [] -> assert false
  | x::xs -> List.fold_left f x xs
;;
let max = foldl1 (fun x y -> if x > y then x else y) ;;
let min = foldl1 (fun x y -> if x < y then x else y) ;;

let expand tables (package,version) (name,v) =
  List.filter_map (fun x ->
      (* we filter out package that expand to itselves *)
      (* XXX *)
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
        |Some((`Gt|`Geq) as op,v) ->
            begin match get_versions tables (name,v) with
            |[] -> []
            |l -> [(CudfAdd.encode name, Some(op,min l))] end
        |Some((`Lt|`Leq) as op,v) ->
            begin match get_versions tables (name,v) with
            |[] -> []
            |l -> [(CudfAdd.encode name, Some(op,max l))] end
      ) l
    )
  )

let loadlp tables (package,version) l =
  List.unique (
    List.flatten (
      List.map (fun (name,sel) ->
        match CudfAdd.cudfop sel with
        |None  -> [(CudfAdd.encode name, None)]
        (* we normalize everything to equality *)
        |Some(`Eq,v) -> expand tables (package,version) (name,v)
        |Some(_,v) -> (
            Util.print_warning "selector in provides for package %s" package;
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
  let (n,v) = (pkg.name,pkg.version) in
  let fileconflicts = Hashtbl.find_all tables.fileconflicts (n,v) in 
  { Cudf.default_package with
    Cudf.package = CudfAdd.encode pkg.name ;
    Cudf.version = get_version tables (n,v) ;
    Cudf.depends = loadll tables pkg.depends ;
    Cudf.conflicts = List.unique (
      (loadl tables pkg.conflicts) @
      (loadl tables fileconflicts)
    );
    Cudf.provides = List.unique (
      (loadlp tables (n,v) pkg.provides) @
      (loadp_files tables (n,v)) 
    );
    Cudf.installed = inst 
  }

let load_list l =
  let timer = Util.Timer.create "Rpm.load_list" in
  Util.Timer.start timer;
  let tables =  init_tables l in
  let pkglist = List.map (tocudf tables) l in
  clear tables;
  Util.Timer.stop timer pkglist

let load_universe l =
  let pkglist = load_list l in
  let timer = Util.Timer.create "Rpm.load_universe" in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
