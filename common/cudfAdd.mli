(** Library of additional functions for the CUDF format. *)

(** {2 Basic comparison operations for packages} *)

(** Equality test: two CUDF packages are equal if their names and versions are equal. *)
val equal : Cudf.package -> Cudf.package -> bool

(** Compare function: compares two CUDF packages using standard CUDF comparison operator (i.e. comparing by their name and version). *)
val compare : Cudf.package -> Cudf.package -> int

(** {2 Specialized data structures for CUDF packages} *)

(** A hash function for CUDF packages, using only their name and version. *)
val hash : Cudf.package -> int

(** Data structures: *)

(** Specialized hashtable for CUDF packages. *)
module Cudf_hashtbl : (Hashtbl.S with type key = Cudf.package)

(** Specialized set data structure for CUDF packages. *)
module Cudf_set : (Set.S with type elt = Cudf.package)

(** Convert a list of CUDF packages to a set of CUDF packages. *)
val to_set : Cudf_set.elt list -> Cudf_set.t

(** {2 Functions to encode and decode strings. } *)
(* TODO: What are these functions doing in this module? *)

(** Encode a string.

    Replaces all the "not allowed" characters
    with their ASCII code (in hexadecimal format),
    prefixed with a '%' sign.
    
    Only "allowed" characters are letters, numbers and these: [@/+().-],
    all the others are replaced.
    
    Examples:
    {ul
    {li [encode "ab"  = "ab"]}
    {li [encode "|"   = "%7c"]}
    {li [encode "a|b" = "a%7cb"]}
    }
*)
val encode : string -> string

(** Decode a string. Opposite of the [encode] function.

    Replaces all the encoded "not allowed" characters
    in the string by their original (i.e. not encoded) versions.

    Examples:
    {ul
    {li [decode "ab" = "ab"]}
    {li [decode "%7c" = "|"]}
    {li [decode "a%7cb" = "a|b"]}
    }
*)
val decode : string -> string


(** {2 Formatting, printing, converting to string. } *)

val string_of : (Format.formatter -> 'a -> 'b) -> 'a -> string

val pp_version : Format.formatter -> Cudf.package -> unit
val pp_package : Format.formatter -> Cudf.package -> unit

val string_of_version : Cudf.package -> string
val string_of_package : Cudf.package -> string

module StringSet : (Set.S with type elt = ExtLib.String.t)
val pkgnames : Cudf.universe -> StringSet.t


(** {2 Additional functions on the CUDF data types. } *)

(** Returns a list of packages containing for each package only the
    latest version *)
val latest: Cudf.package list -> Cudf.package list

val add_properties :
  Cudf.preamble -> (string * Cudf_types.typedecl1) list -> Cudf.preamble
val is_essential : Cudf.package -> bool

(** build a hash table that associates (package name, String version) to CUDF packages *)
val realversionmap :
  Cudf.package list ->
  (Cudf_types.pkgname * string, Cudf.package) ExtLib.Hashtbl.t

val vartoint : Cudf.universe -> Cudf.package -> int
val inttovar : Cudf.universe -> int -> Cudf.package

val add_to_package_list :
  ('a, 'b list ref) ExtLib.Hashtbl.t -> 'a -> 'b -> unit
val get_package_list : ('a, 'b list ref) ExtLib.Hashtbl.t -> 'a -> 'b list
val unique : 'a list -> 'a list
val normalize_set : int list -> int list
val who_provides :
  Cudf.universe ->
  Cudf_types.pkgname * Cudf_types.constr -> Cudf.package list
val resolve_vpkg_int :
  Cudf.universe -> Cudf_types.pkgname * Cudf_types.constr -> int list
val resolve_vpkgs_int :
  Cudf.universe -> (Cudf_types.pkgname * Cudf_types.constr) list -> int list
val resolve_deps :
  Cudf.universe ->
  (Cudf_types.pkgname * Cudf_types.constr) list -> Cudf.package list
val who_depends : Cudf.universe -> Cudf.package -> Cudf.package list list
val who_conflicts :
  (int, int list ref) ExtLib.Hashtbl.t ->
  Cudf.universe -> Cudf.package -> Cudf.package list
val init_conflicts : Cudf.universe -> (int, int list ref) ExtLib.Hashtbl.t
val compute_pool : Cudf.universe -> int list list array * int list array
val cudfop :
  (string * string) option ->
  ([> `Eq | `Geq | `Gt | `Leq | `Lt ] * string) option

(** [pp ?decode from_cudf pkg] package pretty printer.
    [from_cudf] a function that gets a (name,cudfversion) pair and returns a 
    (name,realversion).
    [?decode] a function that decode the package name and version
    
    returns : a pair (name,versiom,property list)

    note that if the package has version less then 0, then the version is printed
    as "nan"
*)
val pp :
  (Cudf_types.pkgname * Cudf_types.version -> 'a * Cudf_types.pkgname) ->
  ?decode:(Cudf_types.pkgname -> string) ->
  Cudf.package -> string * string * (string * string) list
