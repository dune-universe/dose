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

let wrap f = try f () with End_of_file -> raise IO.No_more_input

IFDEF HASZIP THEN
let gzip_open_file file =
  let ch = Gzip.open_in file in
  IO.create_in
  ~read:(fun () -> wrap (fun _ -> Gzip.input_char ch))
  ~input:(wrap (fun _ -> Gzip.input ch))
  ~close:(fun () -> Gzip.close_in ch)
;;
END

IFDEF HASBZ2 THEN
(*
 * Almost there , but not quite
let bzip_open_in file =
  let ch = Bz2.open_in file in
  IO.create_in
  ~read:(fun () -> Bz2.input_char ch)
  ~input:(Gzip.input ch)
  ~close:(fun () -> Bz2.close_in ch)
*)

let bzip_open_file file = failwith "Not Yet implemented"
END

let std_open_file file = IO.input_channel (open_in file)
let open_ch ch = IO.input_channel ch
let close_ch ch = IO.close_in ch

let open_file file =
  if Filename.check_suffix file ".gz" then
IFDEF HASZIP THEN
    gzip_open_file file
ELSE
    failwith "gzip not supported"
END
  else 
  if Filename.check_suffix file ".bz2" then
IFDEF HASBZ2 THEN
    bzip_open_file file
ELSE
    failwith "bzip not supported"
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
