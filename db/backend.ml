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

(** database backend *)

open Printf
open ExtLib
open Json_type
open Idbr 
open Sql

let cnf_assoc_relations = [
  `Depends,"depends";
  `Pre_depends,"pre_depends";
  `Recommends,"recommends";
  `Suggests,"suggests";
  `Enhances,"enhances"
]

let conj_assoc_relations = [
  `Conflicts,"conflicts";
  `Provides,"provides";
  `Replaces,"replaces";
  `Breaks,"breaks"; 
]

(* Default set of relations to be considered *)
let relations = [`Conflicts;`Depends;`Pre_depends;`Provides;(* `Breaks *)]

type database = {
  relations : Idbr.relation list ;
  extras : string list ;
  connection : Sql.dbraw ;
}

let createdb relations extras connection =
  { relations = relations ; extras = extras ; connection = connection }

let reverse l = List.map (fun (x,y) -> (y,x)) l
let assoc_rels = conj_assoc_relations @ cnf_assoc_relations

let reverse_conj = reverse conj_assoc_relations
let reverse_cnf = reverse cnf_assoc_relations

let relation_to_string r = List.assoc r assoc_rels

(* ------------------------------------------------------------------ *)

let unpack_json = function
  |Array [String n; Null; Null] -> (n,"","")
  |Array [String n; String s; String v] -> (n,s,v)
  |_ -> assert false

(** unpacks the json string stored in the db and returns a compact
    representation json independent (list of list of triples) *)
let unpack_json_cnf_depends = function
  |None -> []
  |Some(input) ->
    Json_type.Browse.list (fun x ->
       Json_type.Browse.list unpack_json x)
    (Json_io.json_of_string input)

(** unpacks the json string stored in the db and returns a compact
    representation json independent (list of triples) *)
let unpack_json_conj_depends = function
  |None -> []
  |Some(input) -> 
       Json_type.Browse.list unpack_json
       (Json_io.json_of_string input)

let default_hd = 
  ["name";"number";"version_id"] @
  (List.map fst (reverse_cnf @ reverse_conj))
;;

(* from Jason to Idbr.package *)
let pack (name,number,id) row header =
  let i = ref 0 in
  let init = {
    name = name;
    number = number;
    version_id = id;
    cnf_deps = [];
    conj_deps = [];
    extra = []; }
  in
  Array.fold_left (fun s hd ->
    let pkg = 
      if List.mem_assoc hd reverse_conj then (
        let t = List.assoc hd reverse_conj in
        let dep = (t, unpack_json_conj_depends row.(!i)) in
        { s with conj_deps=dep::s.conj_deps }
      )
      else if List.mem_assoc hd reverse_cnf then (
        let t = List.assoc hd reverse_cnf in
        let dep = (t, unpack_json_cnf_depends row.(!i)) in
        { s with cnf_deps=dep::s.cnf_deps }
      ) 
      else if not (List.mem hd default_hd) then begin
        try { s with extra = (hd,Option.get row.(!i))::s.extra }
        with Not_found -> s
      end 
      else s
    in incr i ; pkg
  ) init header

let build (row,header) =
  let name = Option.get row.(0) in
  let number = Option.get row.(1) in
  let version_id = int_of_string (Option.get row.(2)) in
  (pack (name, number, version_id) row header)

(** create a list of [Idbr.pkg] *)
let list rows = List.map build rows

(* iter the function [f] on the [Sql3.rows] that are result
   of a query, the function  *)
let iter f rows = List.iter (fun hr -> f (build hr)) rows

let map f rows = List.map (fun hr -> f (build hr)) rows

(** create a list of [Idbr.pkg] objects *)
let has rows =
  let ht = Hashtbl.create 25000 in
  iter (fun p -> Hashtbl.add ht p.version_id p) rows

let print rows =
  iter (fun p ->
    Printf.printf "%d" p.version_id
    (* FIXME: print deps ! *)
  ) rows

(****************************************************************)

type db_selector = ( string * string * string * string )
type row = (string option array * string array)
type row_no_header = string option array

let compile_query query = 
  let rec aux = function
    |`Or [] | `And [] | `All -> None
    |`And l -> Some("(" ^ (String.concat " and " (List.filter_map aux l)) ^ ")")
    |`Or l -> Some("(" ^ (String.concat " or " (List.filter_map aux l)) ^ ")")

    |`Suite s -> Some(Printf.sprintf "suite = '%s'" s)
    |`Comp s  -> Some(Printf.sprintf "comp = '%s'" s)
    |`Arch s  -> Some(Printf.sprintf "arch = '%s'" s)
    |`Date s  -> Some(Printf.sprintf "timestamp = '%s'" s)
    |`Interval (s1,s2)  -> Some(Printf.sprintf "timestamp BETWEEN '%s' and '%s'" s1 s2)

    |`Section s -> Some(Printf.sprintf "section = '%s'" s)
    |`Essential true -> Some(Printf.sprintf "essential = 1")
    |`Essential false -> Some(Printf.sprintf "essential = 0")
    |`Priority s -> Some(Printf.sprintf "priority = '%s'" s)

    |`Version (_,"") -> None
    |`Version ("<<",s) -> Some(Printf.sprintf "number < '%s'" s)
    |`Version (">>",s) -> Some(Printf.sprintf "number > '%s'" s)
    |`Version (sel,s) -> Some(Printf.sprintf "number %s '%s'" sel s)

    |`Version_id i -> Some(Printf.sprintf "version_id = %d" i)

    |`Number s -> Some(Printf.sprintf "number = '%s'" s)
    |`Name s -> Some(Printf.sprintf "name = '%s'" s)

  in
  let r = aux query in
  if Option.is_none r then "where true"
  else "where " ^ (Option.get r)

let map_concat f l =
  if List.length l = 0 then ""
  else
    let sl = List.map(fun r -> f r) l
    in ", " ^ (String.concat "," sl)

(** create a commodity temporary sql view to simplify all successive queries *)
let create_view_all_packages db query =
  let sql_rel = 
    let f r =
      let s = List.assoc r assoc_rels in
      Printf.sprintf "version.%s as %s" s s
    in
    map_concat f db.relations
  in
  let sql_extras = 
    let f s = Printf.sprintf "info.%s as %s" s s in
    map_concat f db.extras
  in
  let q = compile_query query in
  (* select distinct on (version.id) *)
  let sql = Printf.sprintf "
create temp view all_t as
select 
version.name as name,
version.number as number,
version.id as version_id,
aptlist.id as aptlist_id,
aptlist.timestamp as timestamp,
packages.suite as suite,
packages.arch as arch,
packages.comp as comp %s %s
from version,aptlist,interval,packages,info %s and
version.id = interval.version_id and 
interval.aptlist_id = aptlist.id and 
packages.id = aptlist.packages_id and
info.id = version.info_id
order by version.id,name,number;
" sql_rel sql_extras q
  in
  !Sql.database.exec_no_result db.connection sql
;;


(** create a temporary sql table and indexes to hold the 
  current package selection. The table will be removed at exit. *)
let create_view_universe db =
  let sql = Printf.sprintf "create temp table universe as Select * from all_t" in
  (* Common.Util.print_info "%s" sql ; *)
  let version_idx = "create index version_idx on universe (version_id)" in
  !Sql.database.exec_no_result db.connection sql;
  !Sql.database.exec_no_result db.connection version_idx
;;

let late_binding = function
  |"pgsql" ->
      IFDEF PGSQL THEN Pgsql.load () ELSE failwith "pgsql not supported" END
  |"sqlite" ->
      IFDEF SQLITE THEN Sqlite.load () ELSE failwith "sqlite not supported" END
  |_ -> failwith "DB late binding failed"
;;

let open_database dbtype (user,pass,host,port,dbname) =
  ignore(late_binding dbtype) ;
  let db = !Sql.database.open_db (user,pass,host,port,dbname) in
  createdb [] [] db
;;

(** initialize the db *)
let init_database ?(extras=[]) ?(relations=relations) db query =
(*  begin try dynlink_db dbtype with Dynlink.Error(e) -> failwith (Dynlink.error_message e) end ; *)
  let db = { db with relations = relations ; extras = extras } in
  create_view_all_packages db query;
  create_view_universe db;
  db
;;

let clear_database db =
  let s1 = "drop view all_t" in
  let s2 = "drop index version_idx" in
  let s3 = "drop table universe" in
  !Sql.database.exec_no_result db.connection s1;
  !Sql.database.exec_no_result db.connection s2;
  !Sql.database.exec_no_result db.connection s3;
;;

let create_tables db =
begin
  !Sql.database.exec_no_result db.connection
    "CREATE TABLE IF NOT EXISTS file (
       sha1sum VARCHAR PRIMARY KEY UNIQUE,
       timestamp DATESTAMP,
       path VARCHAR
     )";
  !Sql.database.exec_no_result db.connection
    "CREATE TABLE IF NOT EXISTS aptlist (
       id INTEGER PRIMARY KEY,
       timestamp DATESTAMP,
       processed BOOLEAN,
       sha1sum VARCHAR,
       sha1base VARCHAR,
       packages_id INTEGER,
       file_sha1sum INTEGER UNIQUE,
       FOREIGN KEY (file_sha1sum) REFERENCES file(sha1sum)
     )";
  !Sql.database.exec_no_result db.connection
    "CREATE TABLE IF NOT EXISTS packages (
       id INTEGER PRIMARY KEY,
       aptlist_id VARCHAR UNIQUE,
       mirror VARCHAR,
       arch VARCHAR,
       comp VARCHAR,
       suite VARCHAR,
       FOREIGN KEY (aptlist_id) REFERENCES aptlist(id)
     )";
  !Sql.database.exec_no_result db.connection
    "CREATE TABLE IF NOT EXISTS info (
       id INTEGER PRIMARY KEY,
       essential BOOLEAN,
       priority VARCHAR,
       section VARCHAR,
       installed_size INT,
       maintainer VARCHAR,
       architecture VARCHAR,
       source VARCHAR,
       package_size INT,
       build_essential BOOLEAN
     )";
  !Sql.database.exec_no_result db.connection
    "CREATE TABLE IF NOT EXISTS interval (
       version_id INT,
       aptlist_id INT,
       PRIMARY KEY (version_id,aptlist_id),
       FOREIGN KEY (version_id) REFERENCES version(id),
       FOREIGN KEY (aptlist_id) REFERENCES aptlist(id),
       UNIQUE (version_id, aptlist_id)
     )";
  !Sql.database.exec_no_result db.connection
    "CREATE TABLE IF NOT EXISTS version (
       id INTEGER PRIMARY KEY,
       number VARCHAR COLLATE DEBIAN,
       name VARCHAR,
       unit_id INT,
       info_id INT,
       replaces VARCHAR,
       provides VARCHAR,
       pre_depends VARCHAR,
       depends VARCHAR,
       suggests VARCHAR,
       enhances VARCHAR,
       recommends VARCHAR,
       conflicts VARCHAR,
       FOREIGN KEY (info_id) REFERENCES info(id),
       UNIQUE (unit_id, number)
     )";
end;;


(* FIXME : This should be something more explicit *)
exception Error

let check_relations db l = 
  if List.exists(fun r -> not(List.mem r db.relations)) l then begin
    List.iter (fun r -> print_endline (relation_to_string r)) l;
    List.iter (fun r -> print_endline (relation_to_string r)) db.relations;
    raise Error
  end
  else ()

let __select_packages db f query =
  let sql_rel =
    let f r = List.assoc r assoc_rels in
    map_concat f db.relations
  in
  let sql_extra = map_concat (fun x -> x) db.extras in
  let q = compile_query query in
  let sql =
    Printf.sprintf "select name,number,version_id %s %s from universe %s" 
    sql_rel sql_extra q
  in
  f db.connection sql

(** Map the function [f] over the list of all packages. [relations] is a list
  of dependency relation to consider *)
let select_map_packages db f query =
  let proc_func db = !Sql.database.exec_map db (fun r -> fun h -> f (build (r,h))) in
  __select_packages db proc_func query

(** Iter the function [f] over the list of all packages. [relations] is a list
  of dependency relation to consider *)
let select_iter_packages db f query =
  let proc_func db = !Sql.database.exec_iter db (fun r -> fun h -> f (build (r,h))) in
  __select_packages db proc_func query

(** returns the list of all packages in the universe. [relations] is a list
  of dependency relation to consider *)
let select_packages db query =
  __select_packages db (fun db -> !Sql.database.exec db) query

(****************************************************************)

let select_package_by_version_id db relations extras version_id = 
  match 
  select_map_packages db (fun p -> p) (`And [ `Version_id version_id ])
  with
  |[a] -> a
  |_ -> assert false

let select_no_headers db sql =
  match !Sql.database.exec_no_headers db.connection sql with
  |[a] -> Option.get a.(0)
  |_ -> failwith (Printf.sprintf "Db Inconsistency : %s\n" sql)

(** select the unit name give the version id *)
let select_unit_by_version_id db id =
  let sql = Printf.sprintf "select name from universe where version_id = %i" id in
  select_no_headers db sql

(** select the unit name give the version id *)
let select_number_by_version_id db id =
  let sql = Printf.sprintf "select number from universe where version_id = %i" id in
  select_no_headers db sql

(** select the most recent version of a unit in the universe*)
let select_version_by_unit db name =
  let sql = Printf.sprintf "select version_id from universe where name='%s'" name in
  match !Sql.database.exec_no_headers db.connection sql with
  |a::_ -> int_of_string (Option.get a.(0))
  |_ -> raise Not_found

let mem_version_by_unit db name =
  let sql = Printf.sprintf "select count(version_id) from universe where name='%s'" name in
  match !Sql.database.exec_no_headers db.connection sql with
  |[a] when ((int_of_string (Option.get a.(0))) = 0) -> false
  |[a] when ((int_of_string (Option.get a.(0))) >= 1) -> true
  |_ -> assert false

let universe_size db =
  let sql = Printf.sprintf "select count(*) from universe" in
  match !Sql.database.exec_no_headers db.connection sql with
  |[a] -> int_of_string (Option.get a.(0))
  |_ -> assert false

(**************************************************************************)

let select_timestamps db query =
  let q = compile_query query in
  let sql = Printf.sprintf "select timestamp from aptlist %s" q in
  (* Common.Util.print_info "%s" sql ; *)
  let rowl = !Sql.database.exec_no_headers db.connection sql in
  List.fold_left (fun acc a -> (Option.get a.(0))::acc) [] rowl

(**************************************************************************)

let dump_from_query db query =
  select_iter_packages db (fun p ->
    Printf.printf "Package: %s\nVersion: %s\n\n" 
    (select_unit_by_version_id db p.version_id)
    (select_number_by_version_id db p.version_id)
  ) query 

let dump_from_list db pkglist =
  List.iter (fun (pid,_,_) ->
    Printf.printf "Package: %s\nVersion: %s\n\n"
    (select_unit_by_version_id db pid)
    (select_number_by_version_id db pid)
  ) pkglist

(**************************************************************************)
let loadl l =
    List.map (function
        |(id,"","") -> (id,None)
        |(id,op,version) -> (id,Some(op,version))
    ) l

let loadll = List.map loadl

(* ATM we support only debian *)
let todebian (pkg: Idbr.package): Debian.Packages.package =
  { Debian.Packages.name = pkg.name;
    version = pkg.number;
    source = ("",None);
    architecture = "";
    essential = false;
    priority = "";
    depends = loadll (List.assoc (`Depends) pkg.cnf_deps);
    pre_depends = loadll (List.assoc (`Pre_depends) pkg.cnf_deps);
    recommends = [];
    suggests = [];
    enhances = [];
    conflicts = loadl (List.assoc (`Conflicts) pkg.conj_deps);
    breaks = []; (* loadl (List.assoc (`Breaks) pkg.conj_deps); *)
    replaces = [];
    provides = loadl (List.assoc (`Provides) pkg.conj_deps);
    extras = pkg.extra;
  }

(*let from_debian (pkg: Debian.Packages.package): Idbr.package =
  { name = pkg.Debian.Packages.name;
    number = pkg.Debian.Packages.version;
    version_id =
    cnf_deps =
    conj_deps = 
    extra = 
  };;*)

let load_selection db ?(distribution=`Debian) query =
  let module S = Debian.Packages.Set in
  let rawl = select_map_packages db todebian query in
  let s = List.fold_left (fun s x -> S.add x s) S.empty rawl in
  S.elements s

let insert_package db (pkg: Debian.Packages.package) =
  Printf.eprintf "INSERT INTO version (number, name) VALUES ('%s', '%s')"
    pkg.Debian.Packages.name
    pkg.Debian.Packages.version;

(* TODO we should support dynlinking !!! *)
(*
let dynlink_db dbtype =
  let _ = Dynlink.init () in
  match dbtype with
  |"pgsql" -> begin
    if Dynlink.is_native then
      Dynlink.loadfile "_build/db/pgsql.cmxs"
    else begin
      Dynlink.add_interfaces [ "Pgsql" ; "Postgresql" ] [ Sys.getcwd() ; "_build/db/";
      "/usr/lib/ocaml/3.11.0/" ; "/usr/lib/ocaml/3.11.0/postgresql/" ] ;
      Dynlink.loadfile "/usr/lib/ocaml/3.11.0/postgresql/postgresql.cma";
      Dynlink.loadfile "_build/db/pgsql.cmo"
    end
  end
  |"sqlite" -> begin
    if Dynlink.is_native then
      Dynlink.loadfile "_build/db/sqlite.cmxs"
    else begin
      Dynlink.add_interfaces [ "Sqlite" ; "Sqlite3" ] [ Sys.getcwd() ; "_build/db/" ;
      "/usr/lib/ocaml/3.11.0/" ] ;
      Dynlink.loadfile "_build/db/sqlite.cmo"
    end
  end
  |_ -> failwith "Backend not supported"
;;
*)


