(**************************************************************************************)
(*  Copyright (C) 2011 Pietro Abate                                                   *)
(*  Copyright (C) 2011 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(* 
ASPCUD accepted criteria

      Default: none
      Valid:   none, paranoid, -|+<crit>(,-|+<crit>)*
        <crit>: count(<set>) | sum(<set>,<attr>) | unsat_recommends(<set>)
              | aligned(<set>,<attr>,<attr>) | notuptodate(<set>)
        <attr>: CUDF attribute name
        <set> : solution | changed | new | removed | up | down
      For backwards compatibility: 
        new              = count(new)
        removed          = count(removed)
        changed          = count(changed)
        notuptodate      = notuptodate(solution)
        unsat_recommends = unsat_recommends(solution)
        sum(name)        = sum(name,solution)
*)

{
  open Criteria_parser

  let get_regexp lexbuf =
    let open Lexing in
    let c = Lexing.lexeme_char lexbuf 2 in
    let endpos = Bytes.index_from lexbuf.lex_buffer (lexbuf.lex_start_pos + 3) c in
    let len = endpos - (lexbuf.lex_start_pos + 3) in
    let s = Bytes.sub_string lexbuf.lex_buffer (lexbuf.lex_start_pos + 3) len in
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_start_pos + ((String.length s)+4);
    s
}

let lower_letter = [ 'a' - 'z' ]
let upper_letter = [ 'A' - 'Z' ]
let letter = lower_letter | upper_letter
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let blanks = blank+
let symbols = ['.' '_']
let ident = (letter | digit) (letter | digit | symbols)*

rule token = parse
  | "count"             { COUNT }
  | "sum"               { SUM }
  | "unsat_recommends"  { UNSATREC }
  | "aligned"           { ALIGNED }
  | "notuptodate"       { NOTUPDATE }
  | "solution"          { SOLUTION }
  | "changed"           { CHANGED }
  | "new"               { NEW }
  | "removed"           { REMOVED }
  | "up"                { UP }
  | "down"              { DOWN }
  | ":="                { REGEXP (get_regexp lexbuf) }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ','                 { COMMA }
  | ident as s          { IDENT s }
  | blank+              { token lexbuf }
  | eof                 { EOL }

