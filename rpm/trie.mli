
(** simple trie data structure over strings *)

(** the type of the trie where ['a] is the value *)
type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

(** [find key t] return the value associated with [key] in [t] *)
val find : string -> 'a t -> 'a

(** [mem key t] chech if there is a value associated with [key] in [t] *)
val mem : string -> 'a t -> bool

(** [add key value t] *)
val add : string -> 'a -> 'a t -> 'a t

(** [remove key t] *)
val remove : string -> 'a t -> 'a t

(** [prepend s t] creates a new trie where all keys of t are
    prepended with s *)
val prepend : string -> 'a t -> 'a t

(** [restrict p t] return the sub-trie containing only values
    matching keys with with prefix [p] if any otherwise an empty
    trie. Keys in the resulting (not empty) subtree are prepended 
    with [p] *)
val restrict : string -> 'a t -> 'a t

(** [map f t] *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [iter f t] *)
val iter : (string -> 'a -> unit) -> 'a t -> unit

(** [fold f t acc] *)
val fold : (string -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
