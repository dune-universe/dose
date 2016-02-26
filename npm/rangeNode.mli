open RangeDesugar

(** Parse a npm range into a tree
 *
 * Raise: Parse_error
 *
 * *)
val parse : string -> version_expr_sugar

(** Parse a node range into a tree and desugar every special operator of NPM
 *
 * Raise: Parse_error if the input is not a valid npm range
 *        Invalid_argument if t
 *
 * *)
val parse_and_desugar : string -> version_expr


(** Parse NPM's dependency dictionary into a list of tuples where the first
 * element is the name of the depedency and the second one is the range
 *
 * Raise Parse_error if the input is not a valid JSON dictionary
 *       Invalid_argument: If the syntax of the constrains is not a valid range
 * *)
val parse_npm_deps : string -> (string * string) list

(** Takes a JSON dictionary of the dependencies of a package and return a list
 * where each element is a debian dependency expression
 *
 * Raise Parse_error if the input is not a valid JSON dictionary
 *       Invalid_argument: If the syntax of the constrains is not a valid range
 * *)
val npm_to_debian : string -> Pef.Packages_types.vpkg list list
