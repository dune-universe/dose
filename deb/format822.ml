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
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

type loc = Lexing.position * Lexing.position
let dummy_loc: loc = Lexing.dummy_pos, Lexing.dummy_pos
let extend_loc (r1_start, _r1_end) (_r2_start, r2_end) = (r1_start, r2_end)
let loc_of_lexbuf b = (b.Lexing.lex_start_p, b.Lexing.lex_curr_p)

let pp_posfname {
  Lexing.pos_fname = _fname;
  pos_lnum = lnum;
  pos_bol = bol;
  pos_cnum = cnum
} = Printf.sprintf "%s" _fname


let pp_lpos {
  Lexing.pos_fname = _fname;
  pos_lnum = lnum;
  pos_bol = bol;
  pos_cnum = cnum
} = Printf.sprintf "%d:%d" lnum (cnum - bol)

exception Parse_error_822 of string * loc       (* <msg, file, loc> *)
exception Syntax_error of string * loc          (* <msg, file, loc> *)
exception Type_error of string

type deb_parser = { lexbuf: Lexing.lexbuf ; fname: string }

let from_channel ic =
  let f s n = try IO.input ic s 0 n with IO.No_more_input -> 0 in
  { lexbuf = Lexing.from_function f ; fname = "from-input-channel" }

(* since somebody else provides the channel, we do not close it here *)
let parser_wrapper_ch ic _parser = _parser (from_channel ic)

let parse_from_ch _parser ic =
  try parser_wrapper_ch ic _parser
  with 
  |Syntax_error (_msg, (startpos, endpos)) ->
    fatal "Syntax error lines %s--%s:\n%s" (pp_lpos startpos) (pp_lpos endpos) _msg
  | Parse_error_822 (_msg, (startpos, endpos)) ->
    fatal "Parse error lines %s--%s:\n%s" (pp_lpos startpos) (pp_lpos endpos) _msg

type name = string
type version = string
type architecture = string
type buildprofile = string
type vpkgname = (string * architecture option)
type multiarch = [ `Foreign | `Allowed | `None | `Same ]
type source = (name * version option)
type relop = string
type constr = (relop * version)

type vpkg = (vpkgname * constr option)
type vpkglist = vpkg list
type vpkgformula = vpkg list list

type builddep = (vpkg * (bool * architecture) list * (bool * buildprofile) list)
type builddepslist = builddep list
type builddepsformula = builddep list list

type action = I | R
type suite = string
type vpkgreq = (action option * vpkg * suite option)

module RawInput ( Set : Set.S ) = struct
  let input_raw parse files =
    let timer = Util.Timer.create "Debian.Format822.input_raw" in
    Util.Timer.start timer;
    if List.length files > 1 then info "Merging repositories" ;
    let s =
      List.fold_left (fun acc file ->
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
      ) Set.empty files
    in
    info "total packages %n" (Set.cardinal s);
    Util.Timer.stop timer (Set.elements s)

  let input_raw_ch parse ch =
    let timer = Util.Timer.create "Debian.Format822.input_raw_ch" in
    Util.Timer.start timer;
    let s =
      let l = parse "" ch in
      List.fold_left (fun s x -> Set.add x s) Set.empty l
    in
    info "total packages %n" (Set.cardinal s);
    Util.Timer.stop timer (Set.elements s)
end

