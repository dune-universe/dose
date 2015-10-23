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

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type loc = Lexing.position * Lexing.position
type field = (string * (loc * string))
type stanza = field list
type doc = stanza list

let dummy_loc: loc = Lexing.dummy_pos, Lexing.dummy_pos

let error lexbuf msg =
  let curr = lexbuf.Lexing.lex_curr_p in
  let start = lexbuf.Lexing.lex_start_p in
  if curr.Lexing.pos_fname = "" then
    Printf.sprintf "character %d-%d: %s."
      (start.Lexing.pos_cnum - start.Lexing.pos_bol)
      (curr.Lexing.pos_cnum - curr.Lexing.pos_bol)
      msg
  else
    Printf.sprintf
      "File %S, line %d, character %d-%d: %s."
      curr.Lexing.pos_fname
      start.Lexing.pos_lnum
      (start.Lexing.pos_cnum - start.Lexing.pos_bol)
      (curr.Lexing.pos_cnum - curr.Lexing.pos_bol)
      msg

let string_of_loc (start_pos, end_pos) =
  let line { Lexing.pos_lnum = l } = l in
  if line start_pos = line end_pos then
    Printf.sprintf "line: %d" (line start_pos)
  else
    Printf.sprintf "lines: %d-%d" (line start_pos) (line end_pos)

exception Parse_error_822 of string
exception Syntax_error of string
exception Type_error of string

type f822_parser = { lexbuf: Lexing.lexbuf ; fname: string }

let from_channel ic =
  let f s n = try IO.input ic s 0 n with IO.No_more_input -> 0 in
  { lexbuf = Lexing.from_function f ; fname = "from-input-channel" }

(* since somebody else provides the channel, we do not close it here *)
let parser_wrapper_ch ic _parser = _parser (from_channel ic)

let parse_from_ch _parser ic =
  try parser_wrapper_ch ic _parser with 
  |Syntax_error (msg) -> fatal "%s" msg
  |Parse_error_822 (msg) -> fatal "%s" msg

let timer = Util.Timer.create "Format822" 

module RawInput ( Set : Set.S ) = struct

  let input_raw parse files =
    Util.Timer.start timer;
    if List.length files > 1 then info "Merging repositories" ;
    let s =
      List.fold_left (fun acc file ->
        try
          let ch =
           match file with
           (* XXX not sure about this, maybe it should be an option
            * insted of "-" ...  *)
           |"-" -> IO.input_channel stdin 
           |_   -> Input.open_file file
          in 
          let l = parse file ch in
          let _ = Input.close_ch ch in
          List.fold_left (fun s x -> Set.add x s) acc l
        with Input.File_empty -> acc
      ) Set.empty files
    in
    info "total packages %n" (Set.cardinal s);
    Util.Timer.stop timer (Set.elements s)

  let input_raw_ch parse ch =
    Util.Timer.start timer;
    let s =
      let l = parse "" ch in
      List.fold_left (fun s x -> Set.add x s) Set.empty l
    in
    info "total packages %n" (Set.cardinal s);
    Util.Timer.stop timer (Set.elements s)
end
