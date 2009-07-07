(**************************************************************************)
(*  debcudf - Debian Packages file to CUDF conversion tool                *)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib

type t

(** Given a parser returns an associative list containing the name of the
    field in the Packages file and the associated list of strings/lines.
    None if there are no more paragraphs to consider *)
val parse_paragraph : t -> (string * string list) list option

(** Validates a version *)
val parse_version : string -> string

(** Validates a package name *)
val parse_package : string -> string

(** Given a string returns a tuple containing the name of the
    package associated with an options constraint tuple.
    Ex. debhelper (>> 7.0)  == ("debhelper",Some(LT,"7.0")) *)
val parse_constr : string -> string * (string * string) option

(** [parse_vpkglist parse_constr s]
    Given a constraint parsing function (ex. parse_constr ) returns
    a list of parsed constraints *)
val parse_vpkglist : (string -> 'a) -> string -> 'a list
val parse_veqpkglist : (string -> 'a) -> string -> 'a list

(** [parse_vpkgformula parse_constr s] *)
val parse_vpkgformula : (string -> 'a) -> string -> 'a list list

(** [single_line field l] check if the field is on a single line *)
val single_line : string -> string list -> string

(** Given an abstract channel returns a parser instance *)
val start_from_channel : IO.input -> t
