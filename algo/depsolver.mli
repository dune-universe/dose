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

(** Dependency solver. Implementation of the Edos algorithms *)

(** the solver is an abstract data type associated to a universe *)
type solver

(** initialize the solver *)
val load : Cudf.universe -> solver

(** check if the given package can be installed in the universe *)
val edos_install : solver -> Cudf.package -> Diagnostic.diagnosis

(** check if the give package list can be installed in the universe *)
val edos_coinstall : solver -> Cudf.package list -> Diagnostic.diagnosis

(** [univcheck solver] check if all packages in the 
  universe associated with the solver can be installed.
  @param callback : execute a function for each package
  @return the number of broken packages
 *)
val univcheck : ?callback:(Diagnostic.diagnosis -> unit) -> solver -> int

(* [listcheck ~callback:c solver l] check if all packages in [l]
  Invariant : l is a subset of universe
  can be installed in the solver universe.
  @param callback : execute a function for each package
  @return the number of broken packages
 *)

(* val listcheck :
  ?callback:(Diagnostic.diagnosis -> unit) -> solver -> Cudf.package list -> int *)

(** [dependency_closure universe l] compute the dependencies closure 
 * of the give package list. Invariant : l is a subset of universe *)
val dependency_closure : Cudf.universe -> Cudf.package list -> Cudf.package list

(** [reverse_dependencies univ l] compute the reverse dependency list of all
    packages in [l] in the universe [univ] *)
val reverse_dependencies :
  Cudf.universe -> (Cudf.package list) Common.CudfAdd.Cudf_hashtbl.t

val reverse_dependency_closure :
  Cudf.universe -> Cudf.package list -> Cudf.package list
