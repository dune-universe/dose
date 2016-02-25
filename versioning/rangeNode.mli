open RangeDesugar

(** Parse a node range into a tree *)
val parse : string -> version_expr_sugar

(** Parse a node range into a tree and desugar every special operator of NPM*)
val parse_and_desugar : string -> version_expr


(** Parse NPM's dependency dictionary into a list of tuples where the first
 * element is the name of the depedency and the second one is the range
 *
 * Raise Parse_error if the input is not a valid JSON dictionary
 * *)
val parse_npm_deps : string -> (string * string) list

(** Takes a JSON dictionary of the dependencies of a package and return a list
 * where each element is a debian dependency expression *)
val npm_to_debian : string -> string list
