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
  units : (string, (int * string) list) Hashtbl.t;
  files : ((string * string), string) Hashtbl.t;
  fileconflicts : ((string * string), vpkg)  Hashtbl.t;
}

let create n = {
  (* all real packages associated with all versions *)
  units = Hashtbl.create (2 * n);
  (* all files associated to a package that are mentioned as a conflict or depends *)
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

  (* temp_units is the list of all versions in provides or packages *)
  let temp_units = Hashtbl.create (List.length pkglist) in
  (* temp_versions is the list of all version in requires and conflicts *)
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
      match sel with
      |None -> add_units name ""
      |Some(c,v) -> add_units name v
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
      match sel with
      |None -> ()
      |Some(c,v) -> add_versions name v
    ) pkg.conflicts
    ;
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        add_files name;
        match sel with
        |None -> ()
        |Some(c,v) -> add_versions name v
      ) disjunction
    ) pkg.depends
  ) pkglist
  ;

  let initialid = 2 in
  let order l = List.unique (List.sort ~cmp:Version.compare (List.rev l)) in
  Hashtbl.iter (fun name {contents = l1} ->
    (* let l2 = try !(Hashtbl.find temp_versions name) with Not_found -> [] in *)
    let vl = order l1 in
    let (_,m) =
      List.fold_left (fun (i,acc) k ->
        (i+1,(i,k)::acc)
      ) (initialid,[]) vl
    in
    (*
    Util.print_info "package %s : %s" name 
    (List.fold_left (fun acc (i,k) -> Printf.sprintf "%s (%s = %d)" acc k i) "" m); 
    *)
    Hashtbl.add tables.units name m ;
  ) temp_units;

  Hashtbl.clear temp_units;
  Hashtbl.clear temp_files;
  Hashtbl.clear temp_versions;

  tables
;;

let has_release v =
  match Version.parse_version v with
  |(_,_,None) -> false
  |(_,_,Some(_)) -> true

let get_release v =
  match Version.parse_version v with
  |(_,_,Some(r)) -> r
  |_ -> assert false

let get_version v =
  match Version.parse_version v with
  |(_,v,_) -> v
  |_ -> assert false

let get_version_id tables (name,v) =
  try 
    let l = Hashtbl.find tables.units name in
    try fst(List.find (fun (i,k) -> k = v) l)
    with Not_found -> 1
  with Not_found -> 1

(* returns a list (i,k) of all k matching prefix *)
let find_interval prefix vl =
  fst(List.partition (fun (_,k) -> String.starts_with k prefix) vl)

let find_aux f prefix vl =
  let rec aux acc = function
    |[] -> raise Not_found
    |(i,h)::t -> if f acc h then aux h t else i
  in
  aux prefix vl

let find_min prefix vl =
  let f x y = Version.compare x y = -1 in
  find_aux f prefix vl

let find_max prefix vl =
  let f x y = Version.compare x y = 1 in
  find_aux f prefix vl

let build_conflict_constraint tables (name,op,v) =
  match op with
  |`Eq ->
      [(CudfAdd.encode name, Some(`Eq,get_version_id tables (name,v)))]
  |`Gt |`Geq ->
      begin try
        let l = Hashtbl.find tables.units name in
        begin match find_interval (get_release v) l with
        |[] -> [(CudfAdd.encode (name^v), None)]
        |(i,k)::_ when not(has_release k) ->
            let nm = CudfAdd.encode name in
            [(nm, Some(`Lt,i));
             (nm, Some(`Eq, get_version_id tables (name,v)))]
        |(i,k)::_ ->
            let nm = CudfAdd.encode name in
            [(nm, Some(`Eq, get_version_id tables (name,v)))]
        end 
      with Not_found -> [(CudfAdd.encode (name^v), None)] end
  |`Lt |`Leq ->
        let nm = CudfAdd.encode name in
        [(nm, Some(op, get_version_id tables (name,v)))]
  |_ -> assert false

let build_depends_constraint tables (name,op,v) =
  begin try
    let l = Hashtbl.find tables.units name in
    match find_interval v l with
    |[] ->
        begin match l with
        |[] -> [(CudfAdd.encode (name^v), None)]
        |_ ->
            begin match op with
            |`Gt | `Geq -> [(CudfAdd.encode (name), Some(op,find_max v l))]
            |`Lt | `Leq -> [(CudfAdd.encode (name), Some(op,find_min v l))]
            |_ -> [(CudfAdd.encode (name), Some(`Eq,1))]
            end
        end
    |l ->
          let nm = CudfAdd.encode name in
          List.map (fun (i,_) ->
            (CudfAdd.encode name, Some(op,i))
          ) l 
  with Not_found -> [(CudfAdd.encode (name^v), None)] end

let load_provides tables (nm,vr) l =
  List.flatten (
    List.map (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None  -> [(CudfAdd.encode name, None)]
      |Some(_,v) ->
          [(CudfAdd.encode name, Some(`Eq,get_version_id tables (name,v)))]
    ) l
  )
;;

let load_conflicts tables l =
  List.flatten (
    List.map (fun (name,sel) ->
      match CudfAdd.cudfop sel with
      |None  -> [(CudfAdd.encode name, None)]
      |Some(op,v) ->
          if has_release v then
            build_conflict_constraint tables (name,op,v)
          else
            [(CudfAdd.encode name, Some(op,get_version_id tables (name,v)))]
    ) l
  )
;;

let load_depends tables ll =
  List.map (fun l ->
    List.flatten (
      List.map (fun (name,sel) ->
        match CudfAdd.cudfop sel with
        |None  -> [(CudfAdd.encode name, None)]
        |Some(`Eq,v) ->
            let nm = CudfAdd.encode name in
            if has_release v then
              let ver = get_version v in
              [(nm, Some(`Eq,get_version_id tables (name,v)));
               (nm, Some(`Eq,get_version_id tables (name,ver)))]
            else
              build_depends_constraint tables (name,`Eq,v)
        |Some(op,v) ->
              build_depends_constraint tables (name,op,v)
      ) l
    )
  ) ll
;;

let loadp_files tables (name,version) =
  List.unique (
    List.map (fun n -> (n,None)
    ) (Hashtbl.find_all tables.files (name,version))
  )

(* ========================================= *)

let tocudf tables ?(inst=false) pkg =
  let (n,v) = (pkg.name,pkg.version) in
  let fileconflicts = Hashtbl.find_all tables.fileconflicts (n,v) in 
  let name = CudfAdd.encode pkg.name in
  let version = get_version_id tables (n,v) in
  { Cudf.default_package with
    Cudf.package = name ;
    Cudf.version = version ;
    Cudf.depends = load_depends tables pkg.depends ;
    Cudf.conflicts = List.unique (
      (load_conflicts tables pkg.conflicts) (* @
      (loadl tables fileconflicts) *)
    );
    Cudf.provides = List.unique (
      (load_provides tables (n,v) pkg.provides) @
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
