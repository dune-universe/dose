{
open Npm_deps_parser
exception Eof
}
rule token = parse
  | ':' {COLON}
  | ',' {COMMA}
  | "{" {OPEN}
  | "}" {CLOSE}
  | '"' ([^'"']* as s) '"' { CONST s }
