

open ExtLib
open ExtString

type 'a t = Node of ('a option * (char, 'a t) PMap.t)

let empty = Node (None,PMap.empty)

let is_empty = function
  | Node (None, m1) -> PMap.is_empty m1
  | _ -> false

let rec find k t =
  let rec aux  = function
    | [], Node (None,_) -> raise Not_found
    | [], Node (Some v,_) -> v
    | x::r, Node (_,m) -> aux (r,(PMap.find x m))
  in aux(String.explode k,t)

let rec mem k t =
  let rec aux = function
    | [], Node (None,_) -> false
    | [], Node (Some _,_) -> true
    | x::r, Node (_,m) ->
        try aux (r,(PMap.find x m))
        with Not_found -> false
  in aux(String.explode k,t)

let add k v t =
  let rec aux = function
    | [], Node (_,m) -> Node (Some v,m)
    | x::r, Node (v,m) ->
      let t' = try PMap.find x m with Not_found -> empty in
      let t'' = aux (r,t') in
      Node (v, PMap.add x t'' m)
  in
  aux (String.explode k,t)

let prepend k t =
  List.fold_left (fun t x ->
    Node (None, PMap.add x t PMap.empty)
  ) t (List.rev (String.explode k))

let remove k t =
  let rec aux = function
    | [], Node (_,m) -> Node (None,m)
    | x::r, Node (v,m) -> 
      try
        let t' = aux (r,(PMap.find x m)) in
        let m' = if is_empty t' then PMap.remove x m else PMap.add x t' m in
        Node (v, m')
      with Not_found -> t
  in
  aux (String.explode k,t)

let restrict prefix t =
  let rec aux = function
    | x::r, Node (_,m) ->
        let t' = PMap.find x m in
        aux (r,t')
    |[] , t -> prepend prefix t
  in
  try aux (String.explode prefix,t)
  with Not_found -> empty

let rec map f = function
  | Node (None,m)   -> Node (None, PMap.map (map f) m)
  | Node (Some v,m) -> Node (Some (f v), PMap.map (map f) m)

let iter f t =
  let rec aux revp = function
    | Node (None,m) ->
        PMap.iter (fun x -> aux (x::revp)) m
    | Node (Some v,m) ->
        f (String.implode (List.rev revp)) v;
        PMap.iter (fun x t -> aux (x::revp) t) m
  in
  aux [] t

let fold f t acc =
  let rec aux revp t acc =
    match t with
    | Node (None,m) ->
        PMap.foldi (fun x -> aux (x::revp)) m acc
    | Node (Some v,m) ->
        let k = String.implode (List.rev revp) in
        f k v (PMap.foldi (fun x -> aux (x::revp)) m acc)
  in
  aux [] t acc
