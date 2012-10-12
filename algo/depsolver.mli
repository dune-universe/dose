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

(** Dependency solver. Implementation of the Edos algorithms *)

(** the solver is an abstract data type associated to a universe *)
type solver

(** initialize the solver. If [check] is true (default), then check 
    for universe consistency (cf. Cudf_checker.is_consistent) *)
val load : ?check : bool -> Cudf.universe -> solver

(** check if the given package can be installed in the universe 
 
    @param global_constraints : enforce global constraints on the given
    universe. In particular packages marked as `Keep_package must be always
    installed. Default false.

*)
val edos_install : ?global_constraints:bool -> Cudf.universe -> Cudf.package -> Diagnostic.diagnosis

(** check if the give package list can be installed in the universe 
  
    @param global_constraints : enforce global constraints on the given
    universe. In particular packages marked as `Keep_package must be always
    installed. Default false.
*)
val edos_coinstall : ?global_constraints:bool -> Cudf.universe -> Cudf.package list -> Diagnostic.diagnosis

(** accept a list of list of packages and return the coinstallability test of
    the cartesian product. *)
val edos_coinstall_prod : ?global_constraints:bool -> Cudf.universe -> Cudf.package list list -> Diagnostic.diagnosis list

(** remove uninstallable packages from the universe . global_constraints is true
    by default *)
val trim : ?global_constraints:bool -> Cudf.universe -> Cudf.universe

(** return the list of broken packages *)
val find_broken : ?global_constraints:bool -> Cudf.universe -> Cudf.package list

(** return the list of installable packages *)
val find_installable : ?global_constraints:bool -> Cudf.universe -> Cudf.package list

(** [univcheck ] check if all packages in the universe can be installed.
    Since not all packages are directly tested for installation, if a packages
    is installable, the installation might be empty. To obtain an installation
    set for each installable packages, the correct procedure is to iter on the
    list of packages.
 
    @param callback : execute a function for each package. This function can
    have side effects and can be used to collect the installation set or the
    failure reason.

    @param global_constraints : enforce global constraints on the given
    universe. In particular packages marked as `Keep_package must be always
    installed. Default true.

    @return the number of broken packages
 *)
val univcheck : ?global_constraints:bool -> ?callback:(Diagnostic.diagnosis -> unit) -> Cudf.universe -> int

(** [listcheck ~callback:c solver l] check if all packages in [l] can be
   installed.
  
   Invariant : l is a subset of universe can be installed in the solver universe.

   @param callback : execute a function for each package.
   @param global_constraints : enforce global constraints on the given universe.
   @return the number of broken packages
 *)

val listcheck : ?global_constraints:bool -> ?callback:(Diagnostic.diagnosis -> unit) -> 
  Cudf.universe -> Cudf.package list -> int 

(** [dependency_closure universe l] compute the dependencies closure 
    of the give package list.
    Invariant : l must be a subset of universe *)
val dependency_closure : ?maxdepth:int -> ?conjunctive:bool ->
  Cudf.universe -> Cudf.package list -> Cudf.package list

(** [reverse_dependencies univ ] compute the reverse dependency list of all
    packages in the universe [univ] *)
val reverse_dependencies : 
  Cudf.universe -> (Cudf.package list) Common.CudfAdd.Cudf_hashtbl.t

(** [reverse_dependencies_closure univ ] compute the reverse dependency list of all
    packages in [l] in the universe [univ] *)
val reverse_dependency_closure : ?maxdepth:int ->
  Cudf.universe -> Cudf.package list -> Cudf.package list

type enc = Cnf | Dimacs

(** [output_clauses enc univ] return a string encoded accordingly to [enc]
    (default cnf). 

    @param global_constraints : enforce global constraints on the given universe.

  *)
val output_clauses : ?global_constraints:bool -> ?enc:enc -> Cudf.universe -> string

type solver_result =
  |Sat of (Cudf.preamble option * Cudf.universe)
  |Unsat of Diagnostic.diagnosis option
  |Error of string

(** [check_request] check if there exists a solution for the give cudf document 
    if ?cmd is specified, it will be used to call an external cudf solver to
    satisfy the request.
    if ?criteria is specified it will be used as optimization criteria. 
    if ?explain is specified and there is no solution for the give request, the
    result will contain the failure reason.
*)
val check_request : ?cmd : string -> ?criteria : string -> ?explain : bool -> Cudf.cudf -> solver_result
