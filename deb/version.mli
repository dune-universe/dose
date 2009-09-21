(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

(** Debian version comparison function. *)

(** compare two debian versions *)
val compare : string -> string -> int

(** A string representing a debian version is parsed using the following
  bnf grammar.
  {v
  version ::=
   | epoch':'.upstream_version.'-'.debian_revision
   | upstream_version_no_colon.'-'.debian_revision
   | upstream_version_no_colon_no_dash
   | epoch':'.upstream_version_no_dash
  epoch ::= [0-9]+
  upstream_version ::= [a-zA-Z0-9.+-:]+
  upstream_version_no_colon ::= [a-zA-Z0-9.+-]+
  upstream_version_no_dash ::= [a-zA-Z0-9.+:]+
  upstream_version_no_colon_no_dash ::= [a-zA-Z0-9.+]+
  debian_revision ::= [a-zA-Z0-9+.]+
  v}
 *)


