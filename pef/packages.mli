(**************************************************************************************)
(*  Copyright (C) 2015 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2015 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

exception ParseError of string list * string * string
exception IgnorePackage of string

type parse_extras_f = (Common.Format822.field list -> string)

val lexbuf_wrapper :
  ((Lexing.lexbuf -> Packages_parser.token) -> Lexing.lexbuf -> 'a) ->
  Common.Format822.field -> 'a

val assoc : string -> (string * 'a) list -> 'a

val blank_regexp : Re.re
val comma_regexp : Re.re

val parse_name : Common.Format822.field -> Packages_types.name
val parse_version : Common.Format822.field -> Packages_types.version
val parse_vpkg : Common.Format822.field -> Packages_types.vpkg
val parse_vpkglist : Common.Format822.field -> Packages_types.vpkglist
val parse_vpkgformula : Common.Format822.field -> Packages_types.vpkgformula
val parse_archlist : Common.Format822.field -> Packages_types.architecture list
val parse_builddepslist : Common.Format822.field -> Packages_types.builddepslist
val parse_builddepsformula : Common.Format822.field -> Packages_types.builddepsformula

val parse_string : Common.Format822.field -> string
val parse_string_opt : Common.Format822.field -> string option
val parse_string_list : ?rex:Re.re -> Common.Format822.field -> string list

val parse_int : Common.Format822.field -> int
val parse_int_s : Common.Format822.field -> string

val parse_bool : Common.Format822.field -> bool
val parse_bool_s : Common.Format822.field -> string

val parse_s : ?default:'a -> ?required:bool ->
  (Common.Format822.field -> 'a) -> string -> Common.Format822.stanza -> 'a

val parse_e :
  (string * parse_extras_f option) list ->
    Common.Format822.stanza -> (string * string) list

val get_field_value:
  parse:(string -> Common.Format822.stanza -> 'a) -> 
    par:Common.Format822.stanza -> 
      field:(string * 'a option) -> (string * 'a)

class package :
  ?name:string * Packages_types.name option ->
  ?version:string * Packages_types.version option ->
  ?installed:string * Packages_types.installed option ->
  ?depends:string * Packages_types.vpkgformula option ->
  ?conflicts:string * Packages_types.vpkglist option ->
  ?provides:string * Packages_types.vpkglist option ->
  ?recommends:string * Packages_types.vpkgformula option ->
  ?extras:(
    (string * parse_extras_f option) list * 
    (string * string) list option) -> Common.Format822.stanza ->
  object ('a)

    method name : Packages_types.name
    method version : Packages_types.version
    method conflicts : Packages_types.vpkglist
    method depends : Packages_types.vpkgformula
    method provides : Packages_types.vpkglist
    method recommends : Packages_types.vpkgformula
    method installed : Packages_types.installed
    method extras : (string * string) list

    (* val are used in subclasses *)
    val name : (string * Packages_types.name)
    val version : (string * Packages_types.version)
    val conflicts : (string * Packages_types.vpkglist)
    val depends : (string * Packages_types.vpkgformula)
    val provides : (string * Packages_types.vpkglist)
    val recommends : (string * Packages_types.vpkgformula)
    val installed : (string * Packages_types.installed)

    method add_extra : string -> string -> 'a
    method get_extra : string -> string
    method set_extras : (string * string) list -> 'a
    method set_installed : Packages_types.installed -> 'a

    method pp : out_channel -> unit
  end

val parse_package_stanza :
  ((string * ('a * string)) list -> bool) option ->
  (string * ((string * ('a * string)) list -> string) option) list ->
  (string * ('a * string)) list -> package option

val packages_parser :
  string ->
  ((string * (Common.Format822.loc * string)) list -> 'a option) ->
  'a list -> Common.Format822.f822_parser -> 'a list

val parse_packages_in :
  ?filter:((string * (Common.Format822.loc * string)) list -> bool) ->
  ?extras:(string *
           ((string * (Common.Format822.loc * string)) list -> string) option)
          list ->
  string -> IO.input -> package list

module Set :
  sig
    val pkgcompare :
      < name : 'a; version : 'b; .. > ->
      < name : 'a; version : 'b; .. > -> int
    type elt = package
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
val input_raw :
  ?extras:(string *
           ((string * (Common.Format822.loc * string)) list -> string) option)
          list ->
  string list -> Set.elt list
val input_raw_ch :
  ?extras:(string *
           ((string * (Common.Format822.loc * string)) list -> string) option)
          list ->
  IO.input -> Set.elt list
