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

(** Solver output and diagnostic . Low Level API *)

open ExtLib
open Common

type reason =
  |Dependency of (int * Cudf_types.vpkg list * int list)
  |Missing of (int * Cudf_types.vpkg list)
  |Conflict of (int * int * Cudf_types.vpkg)

type result =
  |Success of (?all:bool -> unit -> int list)
  |Failure of (unit -> reason list)

(** There are two types of request. One [Sng] that stays for Single and the
  other [Lst] for List. The first one is used to check the installability
  of one package, the other of a list of packages. For both request you
  can ask to consider (Some globalid) or not the global constraints
  associated to the variable globalid. **)
type request =
  |Sng of (int option * int)
  |Lst of (int option * int list)
