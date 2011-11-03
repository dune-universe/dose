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

(** check if the given package can be installed in the universe *)
val edos_install : Cudf.universe -> solver -> Cudf.package -> Diagnostic.diagnosis

(** check if the give package list can be installed in the universe *)
val edos_coinstall : Cudf.universe -> solver -> Cudf.package list -> Diagnostic.diagnosis

(** remove uninstallable packages from the universe *)
val trim : Cudf.universe -> Cudf.universe

(** [univcheck solver] check if all packages in the 
    universe associated with the solver can be installed.
    Since not all packages
    are directly tested for installation, if a packages is installable, the 
    installation might be empty. To obtain an installation set for
    each installable packages, the correct procedure is to iter on the list of
    packages and use the function [edos_install].
 
    @param callback : execute a function for each package
    @return the number of broken packages
 *)
val univcheck : ?callback:(Diagnostic.diagnosis -> unit) -> Cudf.universe -> int

(* [listcheck ~callback:c solver l] check if all packages in [l]
  Invariant : l is a subset of universe
  can be installed in the solver universe.
  @param callback : execute a function for each package
  @return the number of broken packages
 *)

val listcheck : ?callback:(Diagnostic.diagnosis -> unit) -> 
  Cudf.universe -> Cudf.package list -> int 

(** [dependency_closure universe l] compute the dependencies closure 
    of the give package list.
    Invariant : l must be a subset of universe *)
val dependency_closure : ?maxdepth:int -> ?conjunctive:bool ->
  Cudf.universe -> Cudf.package list -> Cudf.package list

(** [reverse_dependencies univ l] compute the reverse dependency list of all
    packages in [l] in the universe [univ] *)
val reverse_dependencies : 
  Cudf.universe -> (Cudf.package list) Common.CudfAdd.Cudf_hashtbl.t

val reverse_dependency_closure : ?maxdepth:int ->
  Cudf.universe -> Cudf.package list -> Cudf.package list

type enc = Cnf | Dimacs

(** [output_clauses enc univ] return a string encoded accordingly to [enc] *)
val output_clauses : ?enc:enc -> Cudf.universe -> string

(* check if there exists a solution for the give cudf document *)
val check_request : Cudf.cudf_doc -> Diagnostic.diagnosis
