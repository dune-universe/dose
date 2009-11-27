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
module Psql = Postgresql

let conninfo ?(user=None) ?(pass=None) ?(host=None) ?(port=None) ?(dbname=None) () =
  let b = Buffer.create 512 in
  let field name = function
    | None -> ()
    | Some x ->
        Printf.bprintf b "%s='" name;
        for i = 0 to String.length x - 1 do
          if x.[i]='\''
          then Buffer.add_string b "\\'"
          else Buffer.add_char b x.[i]
        done;
        Buffer.add_string b "' "
  in
  field "user" user;
  field "password" pass;
  field "host" host;
  field "port" port;
  field "dbname" dbname;
  Buffer.contents b
;;

let open_db (user,pass,host,port,dbname) =
  let conninfo = conninfo 
    ~user:user
    ~pass:pass
    ~host:host
    ~port:port
    ~dbname:(Some(dbname))
    ()
  in
  try new Psql.connection ~conninfo:conninfo () 
  with (Psql.Error e) as ex -> begin
    Printf.eprintf "psql failed : %s\n" (Psql.string_of_error e);
    raise ex
  end
;;

(* this type annotation is necessary as we use Obj.magic *)
let guarded_exec ( db : Psql.connection ) query =
  try db#exec query
  with (Psql.Error e) as ex -> begin
    match e with
         Psql.Connection_failure msg ->
           Printf.eprintf "psql failed : %s\n" msg;
           raise ex
       | _ ->
           Printf.eprintf "psql failed : %s\n" (Psql.string_of_error e);
           raise ex
  end

(* Transform "" to None in order to respect the Sqlite3 interface 
 * XXX this can be changed *)
let cleanup a = Array.map (function "" -> None |e -> Some(e)) a

let exec_iter db f s =
  let res = guarded_exec db s in
  let hds = res#get_fnames in
  let arr = res#get_all in
  Array.iter (fun row -> f (cleanup row) hds) arr
;;

let exec_iter_no_headers db f s =
  let res = guarded_exec db s in
  let arr = res#get_all in
  Array.iter (fun row -> f (cleanup row)) arr

let exec_map db f s =
  let res = guarded_exec db s in
  let hds = res#get_fnames in
  let arr = res#get_all in
  Array.to_list (Array.map (fun row -> f (cleanup row) hds) arr)

let exec_map_no_headers db f s =
  let res = guarded_exec db s in
  let arr = res#get_all in
  Array.to_list (Array.map (fun row -> f (cleanup row)) arr)

let exec_no_headers db s = exec_map_no_headers db (fun x -> x) s

let exec_no_result db s = exec_iter_no_headers db (fun _ -> ()) s

let exec db s = exec_map db (fun r -> fun h -> (r,h)) s

open Sql

(* XXX this is not type safe as I could load one module, save the dbraw,
 * load another module and then use the dbraw connection with the second module
 * the magic number is used to avoid this situation
 * *)

type t = { db : Psql.connection ; dbobj : int }

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
      close_db = guard (fun db -> db#finish);
      exec_iter = guard exec_iter;

      exec_iter_no_headers = guard exec_iter_no_headers;

      exec_map = (fun db -> guard exec_map db);
      exec_map_no_headers = (fun db -> guard exec_map_no_headers db);

      exec_no_headers = guard exec_no_headers;
      exec_no_result = guard exec_no_result;
      exec = guard exec;
    }
;;

