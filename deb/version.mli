(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Functions for manipulating and comparing Debian version strings.
    Compliant with Debian policy version 3.9.2. *)

(** A string representing a debian version is parsed using the following
  bnf grammar.
  {v
  version ::=
   | epoch':'.upstream_version.'-'.debian_revision
   | upstream_version_no_colon.'-'.debian_revision
   | upstream_version_no_colon_no_dash
   | epoch':'.upstream_version_no_dash
  epoch ::= [0-9]+
  upstream_version ::= [a-zA-Z0-9.+-:~]+
  upstream_version_no_colon ::= [a-zA-Z0-9.+-~]+
  upstream_version_no_dash ::= [a-zA-Z0-9.+:~]+
  upstream_version_no_colon_no_dash ::= [a-zA-Z0-9.+~]+
  debian_revision ::= [a-zA-Z0-9+.~]+
  v}
 *)

(** Comparising according to the debian comparison algorithm *)

(** tell whether two strings define the same versions *)
val equal : string -> string -> bool

(** (compare x y) returns 0 if x is equal to y, a negative integer if x is less than y,
     and a positive integer if x is greater than y. This is consistent with 
     Pervasives.compare. *)
val compare : string -> string -> int

(** Decomposing and recomposing version strings *)
(** split the debian version into its components.
    (epoch,upstream,revision,binnmu) = split v
    v = epoch ^ ":" ^ upstream ^ "-" ^ revision ^ binnmu *)
val split : string -> (string * string * string * string)
val concat : (string * string * string * string) -> string

(* chop the epoch and binnmu component from a version *)
val normalize : string -> string
