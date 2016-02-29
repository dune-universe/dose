%{

(*
range-set  ::= range ( logical-or range ) *
logical-or ::= ( ' ' ) * '||' ( ' ' ) *

range      ::= hyphen | simple ( ' ' simple ) * | ''

hyphen     ::= partial ' - ' partial

simple     ::= primitive | partial | tilde | caret
tilde      ::= '~' partial
caret      ::= '^' partial

primitive  ::= ( '<' | '>' | '>=' | '<=' | '=' | ) partial

partial    ::= xr ( '.' xr ( '.' xr qualifier ? )? )?
xr         ::= 'x' | 'X' | '*' | nr
nr         ::= '0' | ['1'-'9'] ( ['0'-'9'] ) *
qualifier  ::= ( '-' pre )? ( '+' build )?
pre        ::= parts
build      ::= parts
parts      ::= part ( '.' part ) *
part       ::= nr | [-0-9A-Za-z]+

*)

type op = Gte | Lte | Gt | Lt | Eq
type expr =
  | And of (expr * expr)
  | Or of (expr * expr)
  | Version of (op * Versioning.SemverNode.version) option

let incr_str x = string_of_int ((int_of_string x) + 1)
let range v1 v2 =
	And (Version (Some (Gte, v1)), Version (Some (Lt, v2))) 

let normalize_version version =
  match Versioning.SemverNode.parse_raw_version version with
  |(x1, "x", "", pre, build) ->
      let v1 = Versioning.SemverNode.convert (x1,"0","0",pre,build) in
      let v2 = Versioning.SemverNode.convert (incr_str x1,"0","0",pre,build) in
      range v1 v2

  |(x1, "x", "x", pre, build) ->
      let v1 = Versioning.SemverNode.convert (n,"0","0",pre,build) in
      let v2 = Versioning.SemverNode.convert (incr_str n,"0","0",pre,build) in
      range v1 v2

  |(x1, x2, "x", pre, build) ->
      let v1 = Versioning.SemverNode.convert (x1,"0","0",pre,build) in
      let v2 = Versioning.SemverNode.convert (x1,incr_str x2,"0",pre,build) in
      range v1 v2

  |(x1, "", "", pre, build) ->
      let v1 = Versioning.SemverNode.convert (x1,"0","0",pre,build) in
      let v2 = Versioning.SemverNode.convert (incr_str n, "0","0",pre,build) in
      range v1 v2

  |(x1, x2, "", pre, build) ->
      let v1 = Versioning.SemverNode.convert (x1,"0","0",pre,build) in
      let v2 = Versioning.SemverNode.convert (incr_str n, "0","0",pre,build) in
      range v1 v2

  |v -> Version (Some (Eq, Versioning.SemverNode.convert v))

let normalize_tilde version =
  match Versioning.SemverNode.parse_raw_version version with
  | (x1,"","",pre,build) ->
    let v1 = Versioning.SemverNode.convert (x1,"0","0",pre,build) in
    let v2 = Versioning.SemverNode.convert (incr_str x1,"0","0",[],[]) in
    range v1 v2
 
  | (x1,x2,"",pre,build) ->
    let v1 = Versioning.SemverNode.convert (x1,x2,"0",pre,build) in
    let v2 = Versioning.SemverNode.convert (x1,incr_str x2,"0",[],[]) in
    range v1 v2

  | (x1,x2,x3,pre,build) as parsed ->
    let v1 = Versioning.SemverNode.convert parsed in
		let v2 = Versioning.SemverNode.convert (x1,incr_str x2,"0",[],[]) in
	  range parsed v2

let normalize_caret version =
  let parsed = Versioning.SemverNode.parse_raw_version version in
  let two_zeros = ref false in
  let parsed_list =
    match parsed with
    | (x1,x2,x3,_,_) -> [x1; x2; x3]
    | (x1,x2,("x"|"X"|"") -> [x1; x2; "0"]
    | (x1,("x"|"X"|"") -> two_zeros := true; [x1; "0"; "0"]
    | _ -> raise (Invalid_argument "Version not valid")
  in
  let (major,minor,patch) =
    let found = ref false in
    let check_update n = if !found then "0" else (found := true; incr_str n) in
    let generate_range x = if x <> "0" then check_update x else "0" in
    let generated = List.map generate_range parsed_list in
    if List.for_all (fun x -> x = "0") generated then
      if !two_zeros then ("1", "0", "0") 
      else ("0", "1", "0")
      else
        match generated with
        |[x1;x2;x3] -> (x1,x2,x3)
        |_ -> assert false
  in
  let v2 = Versioning.SemverNode.convert (major,minor,patch,[],[]) in
  range v1 v2

let normalize_hypen version1 version2 =
  let v1 = Versioning.SemverNode.parse_version version1 in
  let v2 = Versioning.SemverNode.parse_version version2 in
  range v1 v2

let normalize_primitive op version =
   let v = Versioning.SemverNode.parse_version version in
   Version (Some (string_of_op op, v)) 

let normalize_depend name expr =

let string_of_op = function
  | "<" -> Lt
  | ">" -> Gt
  | "<=" -> Lte
  | ">=" -> Gte
  | "=" -> Eq
  | _ -> assert false

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

%start version, depends, depend
%type <Pef.Packages_types.vpkg> depend
%type <Pef.Packages_types.vpkgformula> depends

%%

depends:
  LCURLY dependlist RCURLY {$2}
;

dependlist:
    depend COMMA dependlist { ($1, $3) :: $5 }
  | depend                  { [($1, $3)] }
  |                         {[]}
;

depend:
  name COLON QUOTE rangelist QUOTE { normalize_depend $1 $3 }

name: QUOTE IDENT QUOTE     { $1 }

rangelist:
    range OR range          { }
  | range                   { }

range:
    hyphen                  { $1 }
  | simplelist              { $1 }
  |                         { [] }

simplelist:
    simple simple           { $1 :: $2 }
  |                         { [] }

simple:
    primitive               { $1 }
  | partial                 { $1 }
  | tilde                   { $1 }
  | caret                   { $1 }

(* here we use IDENT instead of partial as the semantic
   of 1.x - 2.x is not clear *)
hyphen:
  IDENT HYPHEN IDENT     { normalize_hypen $1 $3 }
;

tilde: TILDE IDENT       { normalize_tilde $2 } ;

carret: CARET IDENT      { normalize_caret $2 } ;

primitive: RELOP IDENT   { normalize_primitive $1 $2 } ;

partial: 
    IDENT                { normalize_version $1 }
  | STAR                 { Version None }
  |                      { Version None }
;

%%
 
let version_top = Format822.error_wrapper "version" version
let depends_top = Format822.error_wrapper "depends" depends
let depend_top = Format822.error_wrapper "depend" depend

