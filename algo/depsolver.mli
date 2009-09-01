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

type solver

(** initialize the solver and indexes *)
val init : ?buffer:bool -> Cudf.universe -> solver

(** Low-level : run the solver to satisfy the given request *)
val solve : solver -> Diagnostic.request -> Diagnostic.diagnosis

(** check if the given package can be installed in the universe *)
val edos_install : solver -> Cudf.package -> Diagnostic.diagnosis

(** check if the give package list can be installed in the universe *)
val edos_coinstall : solver -> Cudf.package list -> Diagnostic.diagnosis

(** check if all packages in the Cudf.universe can be installed 
    @param callback : execute a function for each package 
    @return the number of broken packages
*)
val univcheck : ?callback:(Diagnostic.diagnosis -> unit) -> solver -> int

val listcheck :
  ?callback:(Diagnostic.diagnosis -> unit) -> solver -> Cudf.package list -> int

(** compute the dependencies closure (cone) of the give package *)
val dependency_closure : Cudf.universe -> Cudf.package list -> Cudf.package list

val dump : solver -> string
