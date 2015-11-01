
let lexbuf_wrapper s =
  try Criteria_parser.criteria_top Criteria_lexer.token (Lexing.from_string s) with
  |Format822.Syntax_error (m) ->
      let msg = Printf.sprintf "Field %s has a wrong value %s'" m s in
      failwith msg
  |Parsing.Parse_error ->
      let msg = Printf.sprintf "Wrong value: '%s'" s in
      failwith msg
;;

lexbuf_wrapper "+count(solution)" ;;
lexbuf_wrapper "+count(solution,fieldname:=/string/)" ;;
lexbuf_wrapper "+count(solution,fieldname:=%string%)" ;;
lexbuf_wrapper "+count(solution,fieldname:=^string^)" ;;

