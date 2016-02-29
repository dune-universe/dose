{
open Npm_parser

let blank = [ ' ' '\t' ]
let ident = ['0'-'9' 'A'-'Z' 'a'-'z' '.' '+']+
}

rule token = parse
  | "||"									 { OR }
  | '-'										 { HYPHEN }
  | (">=" | "<=") as op    { RELOP op }
  | (">>" | "<<") as op    { RELOP op }
  | ("!=" | "=") as op     { RELOP op }
  | "*"										 { STAR }
  | "~"										 { TILDE }
  | "^"										 { CARET }
  | "\""                   { QUOTE }

  | ':'										 { COLON }
  | ','										 { COMMA }
  | "{"										 { LCURLY }
  | "}"										 { RCURLY }

  | ident as s             { IDENT v }
  | eof                    { EOL }
  | blank+                 { token lexbuf }
  | _ as c                 { Format822.raise_error lexbuf c }
