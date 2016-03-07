{
open Common
open Npm_parser

let inside = ref false

let fix_duplicate_equals op =
  match op with
  | "==" -> "="
  | op2 -> op2

}

let blank = [ ' ' '\t' ]
let hypen = blank+ '-' blank+
let ident = ['0'-'9' 'A'-'Z' 'a'-'z' '.' '+' '-' 'v' '=' '*']+

rule token = parse
  | "||"                { OR }
  | hypen               { HYPHEN }
  | ('>' | '<')   as op { RELOP (Char.escaped op) }
  | (">=" | "<=") as op { RELOP op }
  | ("!=" | "=" | "==") as op  { RELOP (fix_duplicate_equals op) }
  | "(?!\\.)*(?!\\.)"   { STAR }
  | "~"                 { TILDE }
  | "^"                 { CARET }
  | "\""                { QUOTE }

  | ':'                 { COLON }
  | ','                 { COMMA }
  | "{"                 { LCURLY }
  | "}"                 { RCURLY }

  | ident as s          { IDENT s }
  | eof                 { EOL }
  | blank+              { token lexbuf }
  | _ as c              { Format822.raise_error lexbuf c }
