%{
open RangeDesugar
%}
%token EOL
%token OR
%token HYPHEN
%token SPACE
%token STAR
%token TILDE
%token CARET
%token <string> VERSION
%token <string> OPERATOR
%start main
%type <RangeDesugar.version_expr_sugar> main
%%
main:
    comparator EOL {$1}
    | range EOL {$1}
    | version EOL {$1}
;



range:
  set OR range {SOr ($1, $3)}
  | hyphen OR range {SOr ($1, $3)}
  | tilde OR range {SOr ($1, $3)}
  | carret OR range {SOr ($1, $3)}
  | set {$1}
  | hyphen {$1}
  | tilde {$1}
  | carret {$1}
;

set:
  comparator SPACE set {SAnd ($1, $3)} 
  | comparator { $1 }
;

hyphen:
  version SPACE HYPHEN SPACE version {SHyphen ($1, $5)}
  | version HYPHEN version {SHyphen ($1, $3)}
;

tilde:
  TILDE version {STilde $2}
  | TILDE SPACE version {STilde $3}
;

carret:
  CARET version {SCaret $2}
  | CARET SPACE version {SCaret $3}
;

comparator:
  OPERATOR version { string_to_operator($1) $2 }
;


version:
  VERSION HYPHEN VERSION {SVersion (Printf.sprintf "%s-%s" $1 $3)}
  | VERSION {SVersion $1}
  | STAR {SStar}
  | {SStar}
;
