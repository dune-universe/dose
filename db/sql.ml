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

(* database interface *)

type dbraw
type header = string
type headers = header array
type row = string option array
type row_not_null = string array
type sql = string
type conn = 
         string option  (*   - an optional user *)
       * string option  (*   - an optional pass *)
       * string option  (*   - an optional host *)
       * string option  (*   - an optional port *)
       * string         (*   - an optional dbname *)

type db = {
  open_db : conn -> dbraw ;
  close_db : dbraw -> unit ;

  exec_iter : dbraw -> (row -> headers -> unit) -> sql -> unit ;
  exec_iter_no_headers : dbraw -> (row -> unit) -> sql -> unit ;

  exec_map : 'a . dbraw -> (row -> headers -> 'a) -> sql -> 'a list ;
  exec_map_no_headers : 'a . dbraw -> (row -> 'a) -> sql -> 'a list ;

  exec_no_headers : dbraw -> sql -> row list ;
  exec_no_result : dbraw -> sql -> unit ;
  exec : dbraw -> sql -> (row * headers) list ;
}

let dbobj = ref 0 

let database = ref {
  open_db = (fun _ -> failwith "open_db stub") ;
  close_db = (fun _ -> failwith "close_db stub") ;

  exec_iter = (fun _ _ _ -> failwith "exec_iter stub") ;
  exec_iter_no_headers = (fun _ _ _ -> failwith "exec_iter_no_headers stub");

  exec_map = (fun _ _ _ -> failwith "exec_map stub");
  exec_map_no_headers = (fun _ _ _ -> failwith "exec_map_no_headers stub");

  exec_no_headers = (fun _ _ -> failwith "exec_no_headers stub");
  exec_no_result = (fun _ _ -> failwith "exec_no_result stub");
  exec = (fun _ _ -> failwith "exec stub")
}

