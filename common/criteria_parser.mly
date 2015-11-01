%{

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
open Criteria_types

%}

%token <string> IDENT
%token LPAREN RPAREN 
%token COMMA 
%token <string> REGEXP
%token PLUS MINUS
%token EOL
%token COUNT SUM UNSATREC ALIGNED NOTUPDATE 
%token SOLUTION CHANGED NEW REMOVED UP DOWN 

%type <Criteria_types.criteria> criteria_top

%start criteria_top

%%

criteria_top: criteria EOL { $1 }

criteria: 
    predicate { [$1] } 
  | predicate COMMA criteria { $1 :: $3 }

predicate:
    PLUS crit { Maximize($2) } 
  | MINUS crit { Minimize($2) }

crit: 
    COUNT LPAREN set RPAREN { Count($3,None) }
    | COUNT LPAREN set COMMA field RPAREN { Printf.eprintf "SUCCESS\n" ; Count($3,Some $5) }
/*  | SUM LPAREN set COMMA attr RPAREN { Sum($3,$5) } */
  | UNSATREC LPAREN set RPAREN { Unsatrec($3) }
/*  | ALIGNED LPAREN set COMMA attr COMMA attr RPAREN { Aligned($3,$5,$7) } */
  | NOTUPDATE LPAREN set RPAREN { NotUpdate($3) }

set:
    SOLUTION { Solution }
  | CHANGED { Changed }
  | NEW { New }
  | REMOVED { Removed }
  | UP { Up }
  | DOWN { Down }

/* count(S,fieldname:=/string/) */

field: IDENT REGEXP { Printf.eprintf "REG %s %s \n" $1 $2; ($1,$2) }

%%

let error_wrapper f lexer lexbuf =
  let syntax_error msg =
    raise (Format822.Syntax_error (Format822.error lexbuf msg))
  in
  try f lexer lexbuf with
  |Parsing.Parse_error -> syntax_error "RFC 822 parse error"
  |Failure _m -> syntax_error "RFC 822 lexer error"
  |_ -> syntax_error "RFC 822 error"
 
let criteria_top = error_wrapper criteria_top
