open RangeDesugar

module Pcre = Re_pcre

let parse str =
  let lexbuf = Lexing.from_string str in
  Ranges_node_parser.main Ranges_node_lexer.token lexbuf

let parse_and_desugar str =
  let parsed = parse str in
  RangeDesugar.desugar parsed

let parse_npm_deps str =
  let lexbuf = Lexing.from_string str in
  Npm_deps_parser.main Npm_deps_lexer.token lexbuf

let npm_to_debian str =
  let deps = parse_npm_deps str in
  let desugared = List.map (fun (package, const) ->
      (package, parse_and_desugar const)
    ) deps in
  List.map (fun (package, expr) -> RangeDesugar.debian_of_expr package expr) desugared
