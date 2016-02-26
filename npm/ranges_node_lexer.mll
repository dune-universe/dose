(* File lexer.mll *)
{
open Ranges_node_parser
exception Eof
}
rule token = parse
  | "||" {OR}
  | [' ''\t']* { SPACE }
  | '-'  { HYPHEN }
  | ['0'-'9''A'-'Z''a'-'z''.''x''X''+']+  as v { VERSION(v) }
  | '<' { OPERATOR "<"}
  | '>' { OPERATOR ">"}
  | "<=" { OPERATOR "<="}
  | ">=" { OPERATOR ">="}
  | "=" { OPERATOR "="}
  | ['\n'] { EOL }
  | "*" {STAR}
  | "~" {TILDE}
  | "^" {CARET}
