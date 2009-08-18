(* Copyright (C) 2005 Jerome Vouillon

This file is part of Dose2.

Dose2 is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Dose2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

module type S = sig
  type reason (** *)
end

module type T = sig

  module X : S
  type state

  type var = int
  type lit

  (* [lit_of_var] given a variable and its value, it returns a literal.
     The solver assumes that variables are integers numbered between 1 and n. 
     By default the assigment of all variables is Unknown. *)
  val lit_of_var : var -> bool -> lit

  (** initialize the solver *)
  val initialize_problem :
    ?print_var:(Format.formatter -> int -> unit) -> 
      ?buffer: bool -> int -> state

  (** provide a deep copy of the current state of the solver *)
  val copy : state -> state

  val propagate : state -> unit

  val protect : state -> unit

  (** [reset] reset the state of the solver and, in particular, it
      resets the variable assignment array *)
  val reset : state -> unit

  (** the value of a variable at any give time *)
  type value = True | False | Unknown

  (* [assignment] return the array of values associated to every variable *)
  val assignment : state -> value array

  (** [add_un_rule] gets that state of the solver, a literal and 
      reasons to return in case this clause was involved in a clash. 
     *)
  val add_un_rule : state -> lit -> X.reason list -> unit

  (** [add_bin_rule] gets that state of the solver, two literals [a,b] and a
      list reasons to return in case this clause was involved in a clash.
      Updates the internal state of the solver with ( a \lor b ) *)
  val add_bin_rule : state -> lit -> lit -> X.reason list -> unit

  (** [add_bin_rule] gets that state of the solver, a list of literals [l] and a
      list reasons to return in case this clause was involved in a clash.
      Updates the internal state of the solver with ( \Bigvee l ) *)
  val add_rule : state -> lit array -> X.reason list -> unit

  (** [associate_vars] associate a variable to a list of variables. The solver
   *  will use this information to guide the search heuristic *)
  val associate_vars : state -> lit -> var list -> unit

  (** [solve st v] finds a variable assignment that makes [v] true *)
  val solve : state -> var -> bool

  (** [solve st l] finds a variable assignment that makes true all variables in [l]*)
  val solve_lst : state -> var list -> bool

  val collect_reasons : state -> var -> X.reason list
  val collect_reasons_lst : state -> var list -> X.reason list

  val dump : state -> string
end

module M : functor (X : S) -> T with module X = X
