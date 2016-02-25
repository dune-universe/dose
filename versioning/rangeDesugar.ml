
module Pcre = Re_pcre
open SemverNode


type version_item =
  | NumberItem of int
  | XItem
  | EmptyItem

let string_of_version_item i =
  match i with
  | NumberItem n -> string_of_int n
  | XItem -> "x"
  | EmptyItem -> ""

let string_of_version_range (major, minor, patch, pre_list, build_list) =
  let string_of_identifier i =
    match i with
    | NumericIdentifier n -> string_of_int n
    | StringIdentifier x -> x
  in
  let separator_pre acc = if acc = "" then "" else "." in
  let separator_build acc = if acc = "" then "" else "+" in
  let pre = List.fold_left (fun acc x -> Printf.sprintf "%s%s%s" acc (separator_pre acc) (string_of_identifier x)) "" pre_list in
  let build = List.fold_left (fun acc x -> Printf.sprintf "%s%s%s" acc (separator_build acc) x) "" build_list in
  let others =
    match (pre, build) with
    | ("", "") -> ""
    | (x, "") -> Printf.sprintf "-%s" x
    | ("", y) -> Printf.sprintf "+%s" y
    | (x, y) -> Printf.sprintf "-%s+%s" x y
  in
  Printf.sprintf "%s.%s.%s%s" (string_of_version_item major) (string_of_version_item minor) (string_of_version_item patch) others

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

let string_to_operator str next =
  match str with
  | "<" -> SLt next
  | ">" -> SGt next
  | "<=" -> SLte next
  | ">=" -> SGte next
  | "=" -> SEq next
  | _ -> raise (Invalid_argument "Operator not valid")


type version_expr =
  | And of (version_expr * version_expr)
  | Or of (version_expr * version_expr)
  | Version of (version_item * version_item * version_item * identifier list * string list)
  | Gte of version_expr
  | Lte of version_expr
  | Gt of version_expr
  | Lt of version_expr
  | Eq of version_expr


let rec equal_version v1 v2 =
  match (v1, v2) with
  | (SAnd (v11, v12), SAnd (v21, v22)) -> equal_version v11 v21 && equal_version v12 v22
  | (SOr (v11, v12), SOr (v21, v22)) -> equal_version v11 v21 && equal_version v12 v22
  | (SVersion v1, SVersion v2) -> v1 = v2
  | (SStar, SStar) -> true
  | (SGte v1, SGte v2) -> equal_version v1 v2
  | (SLte v1, SLte v2) -> equal_version v1 v2
  | (SGt v1, SGt v2) -> equal_version v1 v2
  | (SLt v1, SLt v2) -> equal_version v1 v2
  | (SEq v1, SEq v2) -> equal_version v1 v2
  | (SHyphen (v11, v12), SHyphen (v21, v22)) -> equal_version v11 v21 && equal_version v12 v22
  | (STilde v1, STilde v2) -> equal_version v1 v2
  | (SCaret v1, SCaret v2) -> equal_version v1 v2
  | (_, _) -> false

let rec string_of_expr expr =
  match expr with
  | SLt v  -> Printf.sprintf "<%s" (string_of_expr v)
  | SGt v  -> Printf.sprintf ">%s" (string_of_expr v)
  | SGte v -> Printf.sprintf ">=%s" (string_of_expr v)
  | SLte v -> Printf.sprintf "<=%s" (string_of_expr v)
  | SVersion v -> v
  | SAnd (v1, v2) -> Printf.sprintf "%s %s" (string_of_expr v1) (string_of_expr v2)
  | SOr (v1, v2) -> Printf.sprintf "%s||%s" (string_of_expr v1) (string_of_expr v2)
  | SStar -> "*"
  | SEq v -> Printf.sprintf "=%s" (string_of_expr v)
  | SHyphen (v1, v2) -> Printf.sprintf "%s - %s" (string_of_expr v1) (string_of_expr v2)
  | STilde v -> Printf.sprintf "~%s" (string_of_expr v)
  | SCaret v -> Printf.sprintf "^%s" (string_of_expr v)


let to_item s =
  match s with
  | "x" -> XItem
  | "X" -> XItem
  | "*" -> XItem
  | v -> NumberItem (int_of_string v)

let try_parse_version v = 
  match parse_version_option true v with
  | Some x -> (NumberItem (x.major), NumberItem (x.minor), NumberItem (x.patch), x.pre, x.build)
  | None ->
      let parts = Pcre.split (Pcre.regexp "\\.") v in
      match List.length parts with
      | 3 -> (to_item (List.nth parts 0), to_item (List.nth parts 1), to_item (List.nth parts 2), [], [])
      | 2 -> (to_item (List.nth parts 0), to_item (List.nth parts 1), EmptyItem, [], [])
      | 1 -> (to_item (List.nth parts 0), EmptyItem, EmptyItem, [], [])
      | _ -> raise (Invalid_argument (Printf.sprintf "Version %s not valid" v))

let get_version v =
  match v with
  | SVersion v2 -> try_parse_version v2
  | _ -> raise (Invalid_argument "There should be a version here")

let desugar_fst_version_hyphen v1 =
  let parsed = get_version v1 in
  match parsed with
  | (EmptyItem, EmptyItem, EmptyItem, pre, build) ->
      (NumberItem 0, NumberItem 0, NumberItem 0, pre, build)
  | (NumberItem x1, EmptyItem, EmptyItem, pre, build) ->
      (NumberItem x1, NumberItem 0, NumberItem 0, pre, build)
  | (NumberItem x1, NumberItem x2, EmptyItem, pre, build) ->
      (NumberItem x1, NumberItem x2, NumberItem 0, pre, build)
  | (NumberItem x1, NumberItem x2, NumberItem x3, pre, build) ->
      (NumberItem x1, NumberItem x2, NumberItem x3, pre, build)
  | _ -> raise (Invalid_argument "Can't desugar the version")

let desugar_snd_version_hyphen v1 =
  let parsed = get_version v1 in
  match parsed with
  | (EmptyItem, EmptyItem, EmptyItem, pre, build) ->
      (NumberItem 1, NumberItem 0, NumberItem 0, pre, build)
  | (NumberItem x1, EmptyItem, EmptyItem, pre, build) ->
      (NumberItem (x1 + 1), NumberItem 0, NumberItem 0, pre, build)
  | (NumberItem x1, NumberItem x2, EmptyItem, pre, build) ->
      (NumberItem x1, NumberItem (x2 + 1), NumberItem 0, pre, build)
  | (NumberItem x1, NumberItem x2, NumberItem x3, pre, build) ->
      (NumberItem x1, NumberItem x2, NumberItem x3, pre, build)
  | _ -> raise (Invalid_argument "Can't desugar the version")


let desugar_x_range v =
  let version = try_parse_version v in
  match version with
  | (EmptyItem, EmptyItem, EmptyItem, pre, build) -> Gte (Version version)
  | (NumberItem x1, XItem, EmptyItem, pre, build) -> 
    let v1 = Version (NumberItem x1, NumberItem 0, NumberItem 0, pre, build) in
    let v2 = Version (NumberItem (x1 + 1), NumberItem 0, NumberItem 0, pre, build) in
    And (Gte v1, Lt v2)
  | (NumberItem x1, XItem, XItem, pre, build) -> 
    let v1 = Version (NumberItem x1, NumberItem 0, NumberItem 0, pre, build) in
    let v2 = Version (NumberItem (x1 + 1), NumberItem 0, NumberItem 0, pre, build) in
    And (Gte v1, Lt v2)
  | (NumberItem x1, NumberItem x2, XItem, pre, build) -> 
    let v1 = Version (NumberItem x1, NumberItem x2, NumberItem 0, pre, build) in
    let v2 = Version (NumberItem x1, NumberItem (x2 + 1), NumberItem 0, pre, build) in
    And (Gte v1, Lt v2)
  | (NumberItem x1, EmptyItem, EmptyItem, pre, build) ->
    let v1 = Version (NumberItem x1, NumberItem 0, NumberItem 0, pre, build) in
    let v2 = Version (NumberItem (x1 + 1), NumberItem 0, NumberItem 0, pre, build) in
    And (Gte v1, Lt v2)
  | (NumberItem x1, NumberItem x2, EmptyItem, pre, build) -> 
    let v1 = Version (NumberItem x1, NumberItem x2, NumberItem 0, pre, build) in
    let v2 = Version (NumberItem x1, NumberItem (x2 + 1), NumberItem 0, pre, build) in
    And (Gte v1, Lt v2)
  | _ -> raise (Invalid_argument (string_of_version_range version))

let desugar_tilde v1 =
  let parsed = get_version v1 in
  let (v1, v2) = match parsed with
    | (NumberItem x1, NumberItem x2, NumberItem x3, pre, build) -> 
      let v2 = ((NumberItem x1), (NumberItem (x2 + 1)), (NumberItem 0), [], []) in
      (parsed, v2)
    | (NumberItem x1, NumberItem x2, EmptyItem, pre, build) -> 
      let v1 = (NumberItem x1, NumberItem x2, NumberItem 0, pre, build) in
      let v2 = (NumberItem x1), (NumberItem (x2 + 1)), (NumberItem 0), [], [] in
      (v1, v2)
    | (NumberItem x1, EmptyItem, EmptyItem, pre, build) -> 
      let v1 = (NumberItem x1, NumberItem 0, NumberItem 0, pre, build) in
      let v2 = (NumberItem (x1 + 1)), (NumberItem 0), (NumberItem 0), [], [] in
      (v1, v2)
    | _ -> raise (Invalid_argument "Version is not valid")
  in
  And ((Gte (Version v1)), (Lt (Version v2)))


let desugar_caret v1 =
  let parsed = get_version v1 in
  let two_zeros = ref false in
  let parsed_list =
    match parsed with
    | (NumberItem x1, NumberItem x2, NumberItem x3, _, _) -> [x1; x2; x3;]
    | (NumberItem x1, NumberItem x2, XItem, _, _)
    | (NumberItem x1, NumberItem x2, EmptyItem, _, _) -> [x1; x2; 0;]
    | (NumberItem x1, EmptyItem, _, _, _)
    | (NumberItem x1, XItem, _, _, _) -> two_zeros := true; [x1; 0; 0]
    | _ -> raise (Invalid_argument "Version not valid")
  in
  let v1 =
    let remove_sugar x =
      match x with
      | NumberItem n -> NumberItem n
      | XItem -> NumberItem 0
      | EmptyItem -> NumberItem 0
    in
    match parsed with
    | (x1, x2, x3, pre, build) -> Version (remove_sugar x1, remove_sugar x2, remove_sugar x3, pre, build)
  in
  let removed =
    let found = ref false in
    let check_update n = if !found then 0 else (found := true; n + 1) in
    let generate_range x = if x <> 0 then check_update x else 0 in
    let generated = List.map generate_range parsed_list in
    if List.for_all (fun x -> x = 0) generated
      then if !two_zeros then [NumberItem 1; NumberItem 0; NumberItem 0] else [NumberItem 0; NumberItem 1; NumberItem 0]
      else List.map (fun x -> NumberItem x) generated
  in
  let v2 = Version (List.nth removed 0, List.nth removed 1, List.nth removed 2, [], []) in
  And (Gte v1, Lt v2)


let rec desugar sugar =
  match sugar with
  | SAnd (v1, v2) -> And (desugar v1, desugar v2)
  | SOr (v1, v2) -> Or (desugar v1, desugar v2)
  | SVersion v -> desugar_x_range v
  | SStar -> Gte (Version (NumberItem 0, NumberItem 0, NumberItem 0, [], []))
  | SGte v -> Gte (desugar v)
  | SLte v -> Lte (desugar v)
  | SGt v -> Gt (desugar v)
  | SLt v -> Lt (desugar v)
  | SEq v -> Eq (desugar v)
  | SHyphen (v1, v2) -> And (Gte (Version (desugar_fst_version_hyphen v1)), Lt (Version (desugar_snd_version_hyphen v2)))
  | STilde v -> desugar_tilde v
  | SCaret v -> desugar_caret v

let rec equal_range r1 r2 =
  match (r1, r2) with
  | (And (v11, v12), And (v21, v22)) -> equal_range v11 v21 && equal_range v12 v22
  | (Or (v11, v12), Or (v21, v22)) -> equal_range v11 v21 && equal_range v12 v22
  | (Version v1, Version v2) -> v1 = v2
  | (Gte v1, Gte v2) -> equal_range v1 v2
  | (Lte v1, Lte v2) -> equal_range v1 v2
  | (Gt v1, Gt v2) -> equal_range v1 v2
  | (Lt v1, Lt v2) -> equal_range v1 v2
  | (Eq v1, Eq v2) -> equal_range v1 v2
  | (_, _) -> false


let rec string_of_range expr =
  match expr with
  | Lt v  -> Printf.sprintf "<%s" (string_of_range v)
  | Gt v  -> Printf.sprintf ">%s" (string_of_range v)
  | Gte v -> Printf.sprintf ">=%s" (string_of_range v)
  | Lte v -> Printf.sprintf "<=%s" (string_of_range v)
  | Version v -> string_of_version_range v
  | And (v1, v2) -> Printf.sprintf "%s %s" (string_of_range v1) (string_of_range v2)
  | Or (v1, v2) -> Printf.sprintf "%s||%s" (string_of_range v1) (string_of_range v2)
  | Eq v -> Printf.sprintf "=%s" (string_of_range v)


let rec debian_of_expr name expr =
  let get_version v =
    match v with
    | Version v -> v
    | _ -> raise (Invalid_argument "There is not a version in this structure")
  in
  let version_str v = string_of_version_range (get_version v) in
  match expr with
  | Lt v -> Printf.sprintf "%s (<< %s)" name (version_str v)
  | Gt v -> Printf.sprintf "%s (>> %s)" name (version_str v)
  | Lte v -> Printf.sprintf "%s (<= %s)" name (version_str v)
  | Gte v -> Printf.sprintf "%s (>= %s)" name (version_str v)
  | Eq v -> Printf.sprintf "%s (= %s)" name (version_str v)
  | Version v -> version_str (Version v)
  | And (x1, x2) -> Printf.sprintf "%s, %s" (debian_of_expr name x1) (debian_of_expr name x2)
  | Or (x1, x2) -> Printf.sprintf "%s | %s" (debian_of_expr name x1) (debian_of_expr name x2)
