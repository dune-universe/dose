/*****************************************************************************/
/*  libCUDF - CUDF (Common Upgrade Description Format/ manipulation library  */
/*  Copyright (C/ 2009-2011  Stefano Zacchiroli <zack@pps.jussieu.fr>        */
/*                                                                           */
/*  Adapted for Dose3 pietro.abate@pps.jussieu.fr                            */
/*                                                                           */
/*  This library is free software: you can redistribute it and/or modify     */
/*  it under the terms of the GNU Lesser General Public License as           */
/*  published by the Free Software Foundation, either version 3 of the       */
/*  License, or (at your option/ any later version.  A special linking       */
/*  exception to the GNU Lesser General Public License applies to this       */
/*  library, see the COPYING file for more information.                      */
/*****************************************************************************/

%{

open ExtLib
(* exception Dup_stanza *)

(* XXX instead of ^ and triggering a reallocation everytime, I could use
 * String.concat to allocate the string of the right size once for all *)
let join (r1, v) (r2, cont) = (Format822.extend_loc r1 r2, v ^ cont)

%}

%token <string * (Format822.loc * string)> FIELD
%token <Format822.loc * string> CONT
%token BLANKLINE EOF
%token PGPHEAD
%type <(string * (Format822.loc * string)) list list> doc_822
%type <(string * (Format822.loc * string)) list option> stanza_822
%type <(string * (Format822.loc * string)) list option> doc_822_sign
%start doc_822 doc_822_sign stanza_822


%%

doc_822_sign:
  | PGPHEAD blanklines field BLANKLINE stanza_822 { $5 }
  | stanza_822 { $1 }
;

doc_822:
  | stanzas             { $1 }
  | blanklines stanzas  { $2 }
;

stanza_822:
  | stanza            { Some $1 }
  | blanklines stanza { Some $2 }
  | blanklines EOF    { None }
  | EOF               { None }
;

blanklines:
  | BLANKLINE            {}
  | BLANKLINE blanklines {}
;

stanzas:
  | stanza EOF          { [ $1 ] }
  | stanza blanklines stanzas { $1 :: $3 }
;

stanza:
  | fields      { (* let keys = List.map fst $1 in
                  (* check for re-defined keys *)
                  if List.length (List.unique keys) < List.length keys then
                    raise Dup_stanza
                  else
                    *)
                    $1
                }
;

fields:
  | field               { [ $1 ] }
  | field fields        { $1 :: $2 }
;

field:
  | FIELD BLANKLINE           { $1 }
  | FIELD BLANKLINE linecont  { let k, v = $1 in k, (join v $3) }
;

linecont:
  | CONT BLANKLINE            { $1 }
  | CONT BLANKLINE linecont   { join $1 $3 }
;

%%

let error_wrapper f =
  fun lexer lexbuf ->
    try f lexer lexbuf
    with
      | Parsing.Parse_error ->
          raise (Format822.Parse_error_822
                   ("RFC 822 (stanza structure) parse error",
                    Format822.loc_of_lexbuf lexbuf))
(*      | Dup_stanza ->
          raise (Format822.Parse_error_822
                   ("duplicate keys in stanza",
                    Format822.loc_of_lexbuf lexbuf))
*)
let doc_822 = error_wrapper doc_822
let stanza_822 = error_wrapper stanza_822

