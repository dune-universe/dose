open ExtLib
 
type attr = string
type set = Solution | Changed | New | Removed | Up | Down
type crit =
  | Count of (set * (string * string) option)
  | Sum of (set * attr)
  | Unsatrec of set
  | Aligned of (set * attr * attr)
  | NotUpdate of set
type predicate = Minimize of crit | Maximize of crit
type criteria = predicate list

