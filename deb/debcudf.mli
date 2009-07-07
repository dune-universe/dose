(*****************************************************************************)
(*  Copyright (C) 2009  <pietro.abate@pps.jussieu.fr>                        *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(** Debian Specific Ipr to Cudf conversion routines *)

open IprLib

(** initialize the version conversion tables *)
val init_tables : Ipr.package list -> unit

val get_version : Ipr.name * Ipr.version -> int

(** convert the a package in the ipr format to cudf. The resulting
    cudf package will be obtained by:
   - Version and package name normalization.
   - Adding self conflicts.
   - Virtual package normalization.
   - Adding priority information.
   - Mapping APT request.
*)
val tocudf : ?inst:bool -> Ipr.package -> Cudf.package

(** load a Cudf universe.
    @param init: skip version table initilization *)
val load_universe : ?init:bool -> Ipr.package list -> Cudf.universe
