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

include Util.Logging(struct let label = __FILE__ end) ;;

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
  let s = " " in
  let ch = Bz2.open_in (open_in file) in
  let input_char ch = 
    try ignore (Bz2.read ch s 0 1) ; s.[0]
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
  if (Unix.stat file).Unix.st_size = 0 then fatal "Input file %s is empty" file;
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

let string_of_opt = function
  | None -> ""
  | Some s -> s
;;

let parse_uri s =
  let url = Url.of_string s in
  let user = url.Url.user 
  and pass = url.Url.passwd 
  and host = url.Url.host 
  and port = url.Url.port
  and query = try Some (List.assoc "query" url.Url.query) with Not_found -> None in
  let db = Printf.sprintf "%s%s" (string_of_opt url.Url.host) url.Url.path
  in
  (url.Url.scheme,(user,pass,host,port,db),query)

let guess_format urilist =
  match List.flatten urilist with
  |uri::l ->
      let (p_default,_,_) = parse_uri uri in
      if List.for_all (fun u -> 
        let (p_list,_,_) = parse_uri u in
        p_default = p_list
      ) l then p_default
      else
        fatal "The input list contains different format prefixes"
  |_ -> fatal "Impossible to guess input format"
