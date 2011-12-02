(***************************************************************************)
(*  Copyright (C) 2010, 2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>    *)
(*                                                                         *)
(*  This library is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU Lesser General Public License as         *)
(*  published by the Free Software Foundation, either version 3 of the     *)
(*  License, or (at your option) any later version.  A special linking     *)
(*  exception to the GNU Lesser General Public License applies to this     *)
(*  library, see the COPYING file for more information.                    *)
(***************************************************************************)

(** Debian architecture terms *)

exception Architectures_inconsistent

val bin_unify: string -> string -> string
val src_matches_bin: string -> string -> bool

