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
let fatal fmt = Util.make_fatal "Input" fmt

IFDEF HASZIP THEN
let gzip_open_file file =
  let ch = Gzip.open_in file in
  let input_char ch = try Gzip.input_char ch with End_of_file -> raise IO.No_more_input in
  let read ch = try Gzip.input ch with End_of_file -> raise IO.No_more_input in
  IO.create_in
  ~read:(fun () -> input_char ch)
  ~input:(read ch)
  ~close:(fun () -> Gzip.close_in ch)
;;
END

IFDEF HASBZ2 THEN
let bzip_open_file file =
  let ch = Bz2.open_in (open_in file) in
  let input_char ch = 
    try let s = " " in ignore (Bz2.read ch s 0 1) ; s.[0]
    with End_of_file -> raise IO.No_more_input
  in
  let read ch s pos len =
    try Bz2.read ch s pos len 
    with End_of_file -> raise IO.No_more_input
  in
  IO.create_in
  ~read:(fun () -> input_char ch)
  ~input:(read ch)
  ~close:(fun () -> Bz2.close_in ch)
;;
END

let std_open_file file = IO.input_channel (open_in file)
let open_ch ch = IO.input_channel ch
let close_ch ch = IO.close_in ch

let open_file file =
  if Filename.check_suffix file ".gz" || Filename.check_suffix file ".cz" then
IFDEF HASZIP THEN
    gzip_open_file file
ELSE
    fatal "gzip not supported. re-configure with --with-zip"
END
  else 
  if Filename.check_suffix file ".bz2" then
IFDEF HASBZ2 THEN
    bzip_open_file file
ELSE
    fatal "bzip not supported. re-configure with --with-bz2"
END
  else 
    std_open_file file
;;

let parse_uri s =
  let opt s = if s <> "" then Some s else None in 
  let url = Url.of_string ~args:[] s in
  let user = opt url.Url.user in
  let pass = opt url.Url.passwd in
  let host = opt url.Url.server in
  let port = if url.Url.port = 0 then None else Some (string_of_int url.Url.port) in
  let query = try Some (List.assoc "query" url.Url.args) with Not_found -> None in
  let db =
    match url.Url.proto with
    |"pgsql" ->
        if (Str.string_before url.Url.file 1) = "/" then
        Str.string_after url.Url.file 1 else url.Url.file
    |_ -> Printf.sprintf "%s%s" url.Url.server url.Url.file
  in
  (url.Url.proto,(user,pass,host,port,db),query)
