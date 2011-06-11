(***************************************************************************)
(*  Copyright (C) 2010 Ralf Treinen <ralf.treinen@pps.jussieu.fr>          *)
(*                                                                         *)
(*  This library is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU Lesser General Public License as         *)
(*  published by the Free Software Foundation, either version 3 of the     *)
(*  License, or (at your option) any later version.  A special linking     *)
(*  exception to the GNU Lesser General Public License applies to this     *)
(*  library, see the COPYING file for more information.                    *)
(***************************************************************************)

(** Debian architecture terms *)

type architecture

val architecture_of_string: string -> architecture
val string_of_architecture: architecture -> string

(** Unification of architecture strings *)
exception Arch_unification_error of string
val arch_unify: architecture -> architecture -> architecture

