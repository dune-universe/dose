
open SemverNode

type version_item =
  | NumberItem of int
  | XItem
  | EmptyItem

type version_expr_sugar =
  | SAnd of (version_expr_sugar * version_expr_sugar)
  | SOr of (version_expr_sugar * version_expr_sugar)
  | SVersion of string
  | SStar
  | SGte of version_expr_sugar
  | SLte of version_expr_sugar
  | SGt of version_expr_sugar
  | SLt of version_expr_sugar
  | SEq of version_expr_sugar
  | SHyphen of (version_expr_sugar * version_expr_sugar)
  | STilde of version_expr_sugar
  | SCaret of version_expr_sugar

type version_expr =
  | And of (version_expr * version_expr)
  | Or of (version_expr * version_expr)
  | Version of (version_item * version_item * version_item * identifier list * string list)
  | Gte of version_expr
  | Lte of version_expr
  | Gt of version_expr
  | Lt of version_expr
  | Eq of version_expr

(** Takes a representation of npm's range sugar and eliminates every special
 * operator *)
val desugar : version_expr_sugar -> version_expr

(** Parses a complete and partial version *)
val try_parse_version : string -> (version_item * version_item * version_item * identifier list * string list)

(** Generate a string of a tree of ranges*)
val string_of_expr : version_expr_sugar -> string

(** Compare two expression tree*)
val equal_range : version_expr -> version_expr -> bool

(** Generate a string of a tree of ranges*)
val string_of_range : version_expr -> string

(** Generate a string of the given version*)
val string_of_version_range : (version_item * version_item * version_item * identifier list * string list) -> string

(** Transform a string into an operator and append the expresion to it*)
val string_to_operator : string -> version_expr_sugar -> version_expr_sugar

(** Transforms a npm's expresion into a debian dependency format. The first
 * string is the name of the package subject to the constrains*)
val debian_of_expr : string -> version_expr -> string
