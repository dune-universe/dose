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

exception Error of string

let string_of_rc = function
  |Sqlite3.Rc.OK -> "Sqlite3.Rc.OK"
  |Sqlite3.Rc.ERROR -> "Sqlite3.Rc.ERROR"
  |Sqlite3.Rc.INTERNAL -> "Sqlite3.Rc.INTERNAL"
  |Sqlite3.Rc.PERM -> "Sqlite3.Rc.PERM"
  |Sqlite3.Rc.ABORT -> "Sqlite3.Rc.ABORT"
  |Sqlite3.Rc.BUSY -> "Sqlite3.Rc.BUSY"
  |Sqlite3.Rc.LOCKED -> "Sqlite3.Rc.LOCKED"
  |Sqlite3.Rc.NOMEM -> "Sqlite3.Rc.NOMEM"
  |Sqlite3.Rc.READONLY -> "Sqlite3.Rc.READONLY"
  |Sqlite3.Rc.INTERRUPT -> "Sqlite3.Rc.INTERRUPT"
  |Sqlite3.Rc.IOERR -> "Sqlite3.Rc.IOERR"
  |Sqlite3.Rc.CORRUPT -> "Sqlite3.Rc.CORRUPT"
  |Sqlite3.Rc.NOTFOUND -> "Sqlite3.Rc.NOTFOUND"
  |Sqlite3.Rc.FULL -> "Sqlite3.Rc.FULL"
  |Sqlite3.Rc.CANTOPEN -> "Sqlite3.Rc.CANTOPEN"
  |Sqlite3.Rc.PROTOCOL -> "Sqlite3.Rc.PROTOCOL"
  |Sqlite3.Rc.EMPTY -> "Sqlite3.Rc.EMPTY"
  |Sqlite3.Rc.SCHEMA -> "Sqlite3.Rc.SCHEMA"
  |Sqlite3.Rc.TOOBIG -> "Sqlite3.Rc.TOOBIG"
  |Sqlite3.Rc.CONSTRAINT -> "Sqlite3.Rc.CONSTRAINT"
  |Sqlite3.Rc.MISMATCH -> "Sqlite3.Rc.MISMATCH"
  |Sqlite3.Rc.MISUSE -> "Sqlite3.Rc.MISUSE"
  |Sqlite3.Rc.NOFLS -> "Sqlite3.Rc.NOFLS"
  |Sqlite3.Rc.AUTH -> "Sqlite3.Rc.AUTH"
  |Sqlite3.Rc.FORMAT -> "Sqlite3.Rc.FORMAT"
  |Sqlite3.Rc.RANGE -> "Sqlite3.Rc.RANGE"
  |Sqlite3.Rc.NOTADB -> "Sqlite3.Rc.NOTADB"
  |Sqlite3.Rc.ROW -> "Sqlite3.Rc.ROW"
  |Sqlite3.Rc.DONE -> "Sqlite3.Rc.DONE"
  |Sqlite3.Rc.UNKNOWN n ->
      "Sqlite3.Rc.UNKNOWN " ^ string_of_int (Sqlite3.Rc.int_of_unknown n)

let open_db (user,pass,host,port,dbname) =
  let db = Sqlite3.db_open dbname in
  let _ = Sqlite3.enable_load_extension db true in 
  let sqliteext =
    let custom = 
      try (Sys.getenv "DEBIANCOLLATE") ^ "/libcollate_debian.so"
      with Not_found -> ""
    in
    let local = "sqlite/libcollate_debian.so" in
    let system = "/usr/lib/sqlite3/libcollate_debian.so" in
    try List.find Sys.file_exists [custom;local;system]
    with Not_found -> begin
      Printf.eprintf "Error: sqlite3 debian collation not found\n" ;
      exit (-1)
    end
  in
  let sql = Printf.sprintf "SELECT load_extension('%s')" sqliteext in
  let rc = Sqlite3.exec db sql in
  match rc with
  | Sqlite3.Rc.OK -> db
  | _ -> raise (Error (string_of_rc rc ^ ": " ^ Sqlite3.errmsg db ))

let exec_iter db f s =
  let rc = Sqlite3.exec db ~cb:f s in
  match rc with
  | Sqlite3.Rc.OK -> ()
  | _ -> raise (Error (string_of_rc rc ^ ": " ^ Sqlite3.errmsg db ^ s))

let exec_iter_no_headers db f s =
  let rc = Sqlite3.exec_no_headers db ~cb:f s in
  match rc with
  | Sqlite3.Rc.OK -> ()
  | _ -> raise (Error (string_of_rc rc ^ ": " ^ Sqlite3.errmsg db ^ s))


(** [exec_list] gets a database handler [db], a sql query [s] and a manipulation 
  function [f] and returns ...*)
let exec_map db f s =
  let stored_result = ref [] in
  let store row header = stored_result := (f row header) :: !stored_result in
  let rc = Sqlite3.exec db ~cb:store s in
  match rc with
  | Sqlite3.Rc.OK -> !stored_result
  | _ -> raise (Error (string_of_rc rc ^ ": " ^ Sqlite3.errmsg db ^ s))

let exec_map_no_headers db f s =
  let stored_result = ref [] in
  let store row = stored_result := f row :: !stored_result in
  let rc = Sqlite3.exec_no_headers db ~cb:store s in
  match rc with
  | Sqlite3.Rc.OK -> !stored_result
  | _ -> raise (Error (string_of_rc rc ^ ": " ^ Sqlite3.errmsg db ^ s))

let exec_no_headers db s = exec_map_no_headers db (fun x -> x) s ;;

let exec db s = exec_map db (fun r -> fun h -> (r,h)) s ;;

let exec_no_result db s = exec_iter_no_headers db (fun _ -> ()) s ;;

open Sql
(* XXX this is not type safe as I could load one module, save the dbraw,
 * load another module and then use the dbraw connection with the second module
 * *)

type t = { db : Sqlite3.db ; dbobj : int }

let load () =
  let dbobj = !Sql.dbobj in
  let _ = incr Sql.dbobj in
  let guard f db =
  let db = Obj.magic db in
  if db.dbobj > 1024 then failwith "Runtime type error: pgsql"
  else f db.db
  in
  Sql.database := {
    open_db = (fun conn -> Obj.magic { db = open_db conn; dbobj = dbobj });
    close_db = (fun _ -> failwith "Not implemented yet");
    exec_iter = guard exec_iter;

    exec_iter_no_headers = guard exec_iter_no_headers;

    exec_map = (fun db -> guard exec_map db);
    exec_map_no_headers = (fun db -> guard exec_map_no_headers db);

    exec_no_headers = guard exec_no_headers;
    exec_no_result = guard exec_no_result;
    exec = guard exec;
  }
;;

