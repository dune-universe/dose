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

(** Dependency solver. Low Level API *)

(** Implementation of the EDOS algorithms (and more).
    This module respects the cudf semantic. 

    This module contains two type of functions.
    Normal functions work on a cudf universe. These are just a wrapper to
    _cache functions.

    _cache functions work on a pool of ids that is a more compact
    representation of a cudf universe based on arrays of integers.
    _cache function can be used to avoid recreating the pool for each
    operation and therefore speed up operations.
*)


(** Sat Solver instance *)
module R : sig type reason = Diagnostic.reason_int end
module S : Common.EdosSolver.T with module X = R

(** internal state of the sat solver. The map allows to transform
    sat solver variables (that must be contiguous) to integers 
    representing the id of a package *)
type solver = { constraints : S.state; map : Common.Util.projection; }

(** Solver Package Pool. [pool_t] is an array where each index
  is an solver variable and the content of the array associates
  cudf dependencies to a list of solver varialbles representing
  a package *)
type dep_t =
    (Cudf_types.vpkg list * int list) list *
    (Cudf_types.vpkg * int list) list
and pool = dep_t array
(** A pool can either be a low level representation of the universe
    where all integers are interpreted as solver variables or a universe
    where all integers are interpreted as cudf package indentifiers *)
and t = [`SolverPool of pool | `CudfPool of pool]

type result =
  | Success of (unit -> int list)
  | Failure of (unit -> Diagnostic.reason_int list)

(** Given a cudf universe , this function returns a [CudfPool]. 
    We assume that cudf uid are sequential and we can use them as an array index *)
val init_pool_univ : global_constraints : bool -> Cudf.universe -> [> `CudfPool of pool]

(** this function creates an array indexed by solver ids that can be 
    used to init the edos solver. Return a [SolverPool] *)
val init_solver_pool : Common.Util.projection -> 
  [< `CudfPool of pool] -> 'a list -> [> `SolverPool of pool]

(** Initalise the sat solver. Operates only on solver ids [SolverPool] *)
val init_solver_cache : ?buffer:bool -> ?explain:bool -> [< `SolverPool of pool] -> S.state

(** Call the sat solver
  
    @param tested: optional int array used to cache older results
*)
val solve : ?tested:bool array -> explain:bool -> solver -> int option * int list -> Diagnostic.result_int

(* [pkgcheck global_constraints callback solver tested id].
   This function is used to "distcheck" a list of packages *)
val pkgcheck : bool -> 
  (Diagnostic.result_int * (int option * int list) -> 'a) option -> 
    bool -> solver -> bool array -> int -> bool

(** Constraint solver initialization
 
    @param buffer debug buffer to print out debug messages
    @param univ cudf package universe
*)
val init_solver_univ : global_constraints: bool -> ?buffer: bool -> 
  ?explain: bool -> Cudf.universe -> solver

(** Constraint solver initialization
 
    @param buffer debug buffer to print out debug messages
    @param pool dependencies and conflicts array idexed by package id
    @param closure subset of packages used to initialize the solver
*)
(* pool = cudf pool - closure = dependency clousure . cudf uid list *)
val init_solver_closure : ?buffer:bool -> [< `CudfPool of pool] -> int list -> solver

(** return a copy of the state of the solver *)
val copy_solver : solver -> solver

(** [reverse_dependencies index] return an array that associates to a package id
    [i] the list of all packages ids that have a dependency on [i].

    @param mdf the package universe
*)
val reverse_dependencies : Cudf.universe -> int list array

val dependency_closure_cache : ?maxdepth:int -> ?conjunctive:bool ->
  [< `CudfPool of pool] -> int list -> S.var list

(** [dependency_closure index l] return the union of the dependency closure of
    all packages in [l] .

    @param maxdepth the maximum cone depth (infinite by default)
    @param conjunctive consider only conjunctive dependencies (false by default)
    @param universe the package universe
    @param pkglist a subset of [universe]
*)
val dependency_closure :
  ?maxdepth:int ->
  ?conjunctive:bool ->
  ?global_constraints:bool ->
  Cudf.universe -> Cudf.package list -> Cudf.package list

(** return the dependency closure of the reverse dependency graph.
    The visit is bfs.    

    @param maxdepth the maximum cone depth (infinite by default)
    @param index the package universe
    @param idlist a subset of [index]

    This function use a memoization strategy.
*)
val reverse_dependency_closure :
  ?maxdepth:int -> int list array -> int list -> int list

(** {2 Progress Bars} *)
val progressbar_init : Common.Util.Progress.t
val progressbar_univcheck : Common.Util.Progress.t
