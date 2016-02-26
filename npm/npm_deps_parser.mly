%token COLON
%token OPEN
%token CLOSE
%token COMMA
%token <string>CONST
%start main
%type <(string * string) list> main
%%
main:
  OPEN dic CLOSE {$2}
;

dic:
  CONST COLON CONST COMMA dic { ($1, $3) :: $5 }
  | CONST COLON CONST { [($1, $3)] }
  | {[]}
;
