(***************************************************************************************)
(*  Copyright (C) 2005-2009 Jerome Vouillon                                            *)
(*  Minor modifications and documentation :                                            *)
(*       Pietro Abate <pietro.abate@pps.jussieu.fr>                                    *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

(** Edos sat solver *)

(** generic failure reason *)
module type S = sig
  type reason
end

(** Sat solver *)
module type T = sig

  (** *)
  module X : S

  (** *)
  type state

  (** variables are integers numbered from 0 to size - 1 *)
  type var = int

  (** the value of a literal *)
  type value = True | False | Unknown

  (** a literal can be positive or negative *)
  type lit

  (** [lit_of_var] given a variable create a positive or a negative literal.
     By default the {b assigment} of all variables (that is its value) is Unknown. *)
  val lit_of_var : var -> bool -> lit

  (** initialize the solver  [initialize_problem n]
      @param ?print_var : a function to print a variable 
      @param ?buffer : decide weather or not to store a human readable
      representaion of the sat problem.
      @param n : the size of the sat problem. that is the max number of
      variables to consider
  *)
  val initialize_problem :
    ?print_var:(Format.formatter -> int -> unit) -> 
      ?buffer: bool -> int -> state

  (** provide a deep copy of the current state of the solver *)
  val copy : state -> state

  val propagate : state -> unit

  val protect : state -> unit

  (** [reset] reset the state of the solver to a state that would be obtained
      by re initializing the solver with an identical constraints set *)
  val reset : state -> unit

  (* [assignment st] return the array of values associated to every variable.*)
  val assignment : state -> value array

  (** [add_un_rule st lit l ] add a literal to the solver associated to a reason
      list. *)
  val add_un_rule : state -> lit -> X.reason list -> unit

  (** [add_bin_rule st a b] add a binary clause to the solver {v a v b v} *)
  val add_bin_rule : state -> lit -> lit -> X.reason list -> unit

  (** [add_bin_rule st l] add a disjuction to the solver of type {% \Bigvee l %} *)
  val add_rule : state -> lit array -> X.reason list -> unit

  (** [associate_vars st lit vl] associate a variable to a list of variables. The solver
      will use this information to guide the search heuristic *)
  val associate_vars : state -> lit -> var list -> unit

  (** [solve st v] finds a variable assignment that makes [v] [True] *)
  val solve : state -> var -> bool

  (** [solve st l] finds a variable assignment that makes [True] all variables in [l]*)
  val solve_lst : state -> var list -> bool

  (** in case of failure return the list of associated reasons *)
  val collect_reasons : state -> var -> X.reason list
  
  (** in case of failure return the list of associated reasons *)
  val collect_reasons_lst : state -> var list -> X.reason list

  (** if the solver was initialized with [buffer = true], 
      dump the state of the solver. Return an empty string otherwise *)
  val dump : state -> string

  (** enable debug messages *)
  val debug : bool -> unit

end

(** *)
module M : functor (X : S) -> T with module X = X
