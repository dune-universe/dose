open RangeDesugar

(** Parse a node range into a tree *)
val parse : string -> version_expr_sugar

(** Parse a node range into a tree and desugar every special operator of NPM*)
val parse_and_desugar : string -> version_expr
