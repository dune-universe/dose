

let parse str =
  let lexbuf = Lexing.from_string str in
  let result = Ranges_node_parser.main Ranges_node_lexer.token lexbuf in
  result


let parse_and_desugar str =
  let parsed = parse str in
  let desugared = RangeDesugar.desugar parsed in
  desugared
