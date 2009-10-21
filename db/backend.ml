(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

(** database backend *)

open Printf
open ExtLib
open Json_type
open Idbr 
open Sql

let debug = ref true

let print_debug s = 
  if !debug then begin
    Printf.eprintf "Backend Debug %s\n" s;
    flush_all()
  end
;;

let cnf_assoc_relations = [
  `Depends,"depends";
  `Pre_depends,"pre_depends";
  `Recommends,"recommends";
  `Suggests,"suggests";
(*  `Breaks,"breaks";*)
  `Enhances,"enhances"
]

let conj_assoc_relations = [
  `Conflicts,"conflicts";
  `Provides,"provides";
  `Replaces,"replaces"
]

(* Default set of relations to be considered *)
let relations = [`Conflicts;`Depends;`Pre_depends;`Provides]

(* XXX: global variable *)
let uni_rels : Idbr.relation list ref = ref []

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

(* from Jason to Idbr.package *)
let pack (name,number,id) row header =
  let i = ref 0 in
  let init = {
    name = name;
    number = number;
    version_id = id;
    cnf_deps = [];
    conj_deps = [] }
  in
  Array.fold_left (fun s hd ->
    let pkg = 
      if List.mem_assoc hd reverse_conj then
        let t = List.assoc hd reverse_conj in
        let dep = (t, unpack_json_conj_depends row.(!i)) in
        { s with conj_deps=dep::s.conj_deps }
      else if List.mem_assoc hd reverse_cnf then
        let t = List.assoc hd reverse_cnf in
        let dep = (t, unpack_json_cnf_depends row.(!i)) in
        { s with cnf_deps=dep::s.cnf_deps }
      else
        s
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

(* XXX: global variable *)
let _db = ref None
let get_db () =
  try Option.get !_db
  with Option.No_value -> failwith "Backend: database not initialized"

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
  if Option.is_none r then ""
  else "where " ^ (Option.get r)

(** create a commodity temporary sql view to simplify all successive queries *)
let create_view_all_packages relations query =
  let sql_rel = match relations with
    |[] -> ""
    |_ -> let l =
      List.map(fun r ->
        let s = List.assoc r assoc_rels in
        Printf.sprintf "version.%s as %s" s s
      ) relations
      in ", " ^ (String.concat "," l)
  in
  let q = compile_query query in
  let sql = Printf.sprintf "
create temp view all_t as
select distinct on (version.id)
version.name as name,
version.number as number,
version.id as version_id,
aptlist.id as aptlist_id,
aptlist.timestamp as timestamp,
packages.suite as suite,
packages.arch as arch,
packages.comp as comp %s
from version,aptlist,interval,packages %s and
version.id = interval.version_id and 
interval.aptlist_id = aptlist.id and 
packages.id = aptlist.packages_id 
order by version.id,name,number;
" sql_rel q
  in
  print_debug sql ;
  !Sql.database.exec_no_result (get_db ()) sql
;;


(** create a temporary sql table and indexes to hold the 
  current package selection. The table will be removed at exit. *)
let create_view_universe () =
  let db = get_db () in
  let sql = Printf.sprintf "create temp table universe as Select * from all_t"
  in
  print_debug sql ;
  let version_idx = "create index version_idx on universe (version_id)" in
  !Sql.database.exec_no_result db sql;
  !Sql.database.exec_no_result db version_idx
;;

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

let late_binding = function
  |"pgsql" ->
      IFDEF PGSQL THEN Pgsql.load () ELSE failwith "pgsql not supported" END
  |"sqlite" ->
      IFDEF SQLITE THEN Sqlite.load () ELSE failwith "sqlite not supported" END
  |_ -> failwith "DB late binding failed"
;;

(** initialize the db *)
let init_database ?(relations=relations) dbtype (user,pass,host,port,dbname) query =
(*  begin try dynlink_db dbtype with Dynlink.Error(e) -> failwith (Dynlink.error_message e) end ; *)
  ignore(late_binding dbtype) ;
  _db := Some(!Sql.database.open_db (user,pass,host,port,dbname)) ;
  uni_rels := relations;
  print_debug "create views and temp tables";
  create_view_all_packages relations query;
  create_view_universe ();
  print_debug "done";
;;

(* FIXME : This should be something more explicit *)
exception Error

let check_relations l = 
  if List.exists(fun r -> not(List.mem r !uni_rels)) l then begin
    List.iter (fun r -> print_endline (relation_to_string r)) l;
    List.iter (fun r -> print_endline (relation_to_string r)) !uni_rels;
    raise Error
  end
  else ()

let __select_versions f query relations =
  check_relations relations;
  let sql_rel = match relations with
    |[] -> ""
    |_ -> let l =
      List.map(fun r ->
        let s = List.assoc r assoc_rels in
        Printf.sprintf "%s" s
      ) relations
      in ", " ^ (String.concat "," l)
  in
  let q = compile_query query in
  let sql = Printf.sprintf "select name,number,version_id %s from universe %s" sql_rel q in
  f (get_db ()) sql

let __select_packages f query relations =
  check_relations relations;
  let sql_rel = match relations with
    |[] -> ""
    |_ -> let l =
      List.map(fun r ->
        let s = List.assoc r assoc_rels in
        Printf.sprintf "%s" s
      ) relations
      in ", " ^ (String.concat "," l)
  in
  let q = compile_query query in
  let sql = Printf.sprintf "select name,number,version_id %s from universe %s" sql_rel q in
  f (get_db ()) sql

(** Map the function [f] over the list of all packages. [relations] is a list
  of dependency relation to consider *)
let select_iter_versions f query relations =
  let proc_func db = !Sql.database.exec_iter db (fun r -> fun h -> f (build (r,h))) in
  __select_versions proc_func query relations

(** Iter the function [f] over the list of all packages matching the give
  selection. [relations] is a list of dependency relation to consider *)
let select_map_versions f query relations =
  let proc_func db = !Sql.database.exec_map db (fun r -> fun h -> f (build (r,h))) in
  __select_versions proc_func query relations

(** returns a list of all package versions matching the give selection. *)
let select_versions query relations =
  __select_versions (fun db -> !Sql.database.exec db) query relations

(** Map the function [f] over the list of all packages. [relations] is a list
  of dependency relation to consider *)
let select_map_packages f query relations =
  let proc_func db = !Sql.database.exec_map db (fun r -> fun h -> f (build (r,h))) in
  __select_packages proc_func query relations

(** Iter the function [f] over the list of all packages. [relations] is a list
  of dependency relation to consider *)
let select_iter_packages f query relations =
  let proc_func db = !Sql.database.exec_iter db (fun r -> fun h -> f (build (r,h))) in
  __select_packages proc_func query relations

(** returns the list of all packages in the universe. [relations] is a list
  of dependency relation to consider *)
let select_packages query relations =
  __select_packages (fun db -> !Sql.database.exec db) query relations

(****************************************************************)

let select_package_by_version_id relations version_id = 
  match 
  select_map_packages (fun p -> p) (`And [ `Version_id version_id ]) relations
  with
  |[a] -> a
  |_ -> assert false

(** select the unit name give the version id *)
let select_unit_by_version_id id =
  let sql = 
    Printf.sprintf "select name from universe where version_id = %i" id
  in
  match !Sql.database.exec_no_headers (get_db ()) sql with
  |[a] -> Option.get a.(0)
  |_ -> Printf.printf "select_unit_by_version_id %d" id ; assert false

(** select the unit name give the version id *)
let select_number_by_version_id id =
  let sql = 
    Printf.sprintf "select number from universe where version_id = %i" id
  in
  match !Sql.database.exec_no_headers (get_db ()) sql with
  |[a] -> Option.get a.(0)
  |_ -> ""

(** select the most recent version of a unit in the universe*)
let select_version_by_unit name =
  let db = get_db () in
  let sql = Printf.sprintf "select version_id from universe where name='%s'" name in
  match !Sql.database.exec_no_headers db sql with
  |a::t -> int_of_string (Option.get a.(0))
  |_ -> raise Not_found

let mem_version_by_unit name =
  let db = get_db () in
  let sql = Printf.sprintf "select count(version_id) from universe where name='%s'" name in
  match !Sql.database.exec_no_headers db sql with
  |[a] when ((int_of_string (Option.get a.(0))) = 0) -> false
  |[a] when ((int_of_string (Option.get a.(0))) >= 1) -> true
  |_ -> assert false

let universe_size () =
  let sql = Printf.sprintf "select count(*) from universe" in
  match !Sql.database.exec_no_headers (get_db ()) sql with
  |[a] -> int_of_string (Option.get a.(0))
  |_ -> assert false

(* we build this table as the number of virtual units is much smaller
 * then the number of real packages in the universe *)
let init_provides () =
  let sql = Printf.sprintf "
    create temp table provides (unit_id INT, version_id INT, number VARCHAR)"
  in ignore(!Sql.database.exec (get_db ()) sql)

(** provides can't be transitive *)
let update_provides () =
  let db = get_db () in
  let extract p = 
    match List.assoc (`Provides) p.conj_deps with
    |[] -> ()
    |l ->
        List.iter (fun (name,sel,ver) ->
          let vl =
            let q = compile_query (`And [ `Name name ; `Version (sel,ver)]) in
            let sql = Printf.sprintf "select version_id, number from universe %s" q in
            List.map (fun r ->
              (int_of_string (Option.get r.(0)), Option.get r.(1))
            ) (!Sql.database.exec_no_headers db sql)
          in
          let sql vid number =
            Printf.sprintf "insert into provides values (%s,%d,'%s')" name vid number
          in
          List.iter (fun (vid,num) ->
            ignore(!Sql.database.exec db (sql vid num))
          ) vl
        ) l
  in
  select_iter_versions extract (`All) [`Provides]

let __expand_provides relations f (name,sel,ver) =
  check_relations relations;
  let sql_rel = match relations with
    |[] -> ""
    |_ -> let l =
      List.map(fun r ->
        let s = List.assoc r assoc_rels in
        Printf.sprintf "%s" s
      ) relations
      in ", " ^ (String.concat "," l)
  in
  let q = compile_query (`And [ `Name name ; `Version (sel,ver)]) in
  let sql = Printf.sprintf "
  select name,number,version_id %s from universe where version_id in (
    select version_id from provides %s)" sql_rel q
  in
  f (get_db ()) sql

let expand_map_provides relations f (id,sel,ver) =
  __expand_provides relations 
  (fun db -> !Sql.database.exec_map db (fun r -> fun h -> f (build (r,h)))) (id,sel,ver)

let expand_iter_provides relations f (id,sel,ver) =
  __expand_provides relations 
  (fun db -> !Sql.database.exec_iter db (fun r -> fun h -> f (build (r,h)))) (id,sel,ver)

let expand_map_versions relations f (name,sel,ver) =
  match
  select_map_versions f (`And [ `Name name ; `Version (sel,ver)]) relations
  with
  |[] -> expand_map_provides relations f (name,sel,ver)
  |l -> l

let expand_iter_versions relations f (name,sel,ver) =
  let empty = ref true in
  let f' x = empty := false ; f x in
  select_iter_versions f' (`And [ `Name name ; `Version (sel,ver)]) relations
  ;
  if !empty then expand_iter_provides relations f (name,sel,ver)
;;

let init_conflicts () =
  let db = get_db () in
  let sql = Printf.sprintf "create temp table conflicts (pkg1 INT, pkg2 INT)" in
  let _idx = "create index conflict_idx on conflicts (pkg1,pkg2)" in
  !Sql.database.exec_no_result db sql;
  !Sql.database.exec_no_result db _idx
;;

let update_conflicts () =
  let db = get_db () in
  let extract p = 
    match List.assoc (`Conflicts) p.conj_deps with
    |[] -> ()
    |l -> List.iter (fun (uid,sel,ver) ->
        let vl = expand_map_versions [] (fun x -> x) (uid,sel,ver) in
        let sql vid =
          Printf.sprintf "replace into conflicts values (%d,%d)" p.version_id vid
        in
        List.iter (fun p -> !Sql.database.exec_no_result db (sql p.version_id)) vl
        ) l
  in
  select_iter_versions extract (`All) [`Conflicts]

let select_conflicts id = 
  let sql =
    Printf.sprintf "select * from conflicts where pkg1 = %d or pkg2 = %d" id id
  in
  let res : Idbr.version_id list ref = ref [] in
  !Sql.database.exec_iter_no_headers (get_db ()) (fun r -> 
    let p1 = int_of_string (Option.get r.(0)) in
    let p2 = int_of_string (Option.get r.(1)) in
    if p1 <> id then res := p1 :: !res else res := p2 :: !res
  ) sql
  ;
  !res

(**************************************************************************)

let dump_from_query query =
  select_iter_packages (fun p ->
    Printf.printf "Package: %s\nVersion: %s\n\n" 
    (select_unit_by_version_id p.version_id)
    (select_number_by_version_id p.version_id)
  ) query []

let dump_from_list pkglist =
  List.iter (fun (pid,_,_) ->
    Printf.printf "Package: %s\nVersion: %s\n\n"
    (select_unit_by_version_id pid)
    (select_number_by_version_id pid)
  ) pkglist


(**************************************************************************)
let loadl l =
    List.map (function
        |(id,"","") -> (id,None)
        |(id,op,version) -> (id,Some(op,version))
    ) l

let loadll = List.map loadl

(* ATM we support only debian *)
let conv pkg =
  { Debian.Packages.name = pkg.name;
    version = pkg.number;
    source = ("",None);
    depends = loadll (List.assoc (`Depends) pkg.cnf_deps);
    pre_depends = loadll (List.assoc (`Pre_depends) pkg.cnf_deps);
    recommends = [];
    suggests = [];
    enhances = [];
    conflicts = loadl (List.assoc (`Conflicts) pkg.conj_deps);
    breaks = loadl (List.assoc (`Breaks) pkg.conj_deps);
    replaces = [];
    provides = loadl (List.assoc (`Provides) pkg.conj_deps);
    extras = [];
  }

let load_selection ?(relations=relations) ?(distribution=`Debian) query =
  let module S = Debian.Packages.Set in
  let rawl = select_map_packages conv query relations in
  let s = List.fold_left (fun s x -> S.add x s) S.empty rawl in
  S.elements s
