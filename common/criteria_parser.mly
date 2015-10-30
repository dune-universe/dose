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

open ExtLib

type attr = string
type set = Solution | Changed | New | Removed | Up | Down
type crit = 
  | Count of set 
  | Sum of (set * attr) 
  | Unsatrec of set 
  | Aligned if (set * attr * attr) 
  | NotUpdate of set
type criteria = Minimize | Maximize
type criteriae = criteria list

%token <string> IDENT
%token LPAREN RPAREN 
%token COMMA
%token PLUS MINUS
%token EOL
(* crit *)
%token COUNT SUM UNSATREC ALIGNED NOTUPDATE 
(* set *)
%token SOLUTION CHANGED NEW REMOVED UP DOWN 

%type <criteriae> criteria

%start criteria

%%

criteriae : 
    criteria { [$1] } 
  | criteria COMMA criteriae { $1 :: $3 }

criteria:
    PLUS crit { Maximize($2) } 
  | MINUS crit { Minimize($2) }

crit: 
    COUNT LPAREN set RPAREN { Count($3) }
  | SUM LPAREN set COMMA attr RPAREN { Sum($3,$5) }
  | UNSATREC LPAREN set RPAREN { Unsatrec($3) }
  | ALIGNED LPAREN set COMMA attr COMMA attr RPAREN { Aligned($3,$5,$7) }
  | NOTUPDATE LPAREN set RPAREN { NotUpdate($3) }

attr:

set:
    SOLUTION { Solution }
  | CHANGED { Changed }
  | NEW { New }
  | REMOVED { Removed }
  | UP { Up }
  | DOWN { Down }

%%
