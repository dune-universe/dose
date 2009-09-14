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

type verbosity = Quiet | Summary | Details
val set_verbosity: verbosity -> unit

(** print a progress bar that increments at each invocation.

    @param ppf
    @param label 
    @param (percentage * total) *)
(* val progress : ?v:verbosity -> string -> int * int -> unit *)

val print_warning : ?ppf:Format.formatter -> ?v:verbosity -> string -> unit
val print_info : ?ppf:Format.formatter -> ?v:verbosity -> string -> unit

(* the following code is borrowed from Cduce. Copyright: Alain Frish *)

val gettimeofday: (unit -> float) ref

(** add a logger *)
val register: verbosity -> (Format.formatter -> unit) -> unit

(** dump the content of all registered loggers to the given formatter *)
val dump: Format.formatter -> unit

module Progress: sig
  type t
    
  val create: string -> t
  val progress : t -> unit
  val enable : string -> unit
  val set_total : t -> int -> unit
  val avalaible : unit -> string list
end

(** counter logger *)
module Counter: sig
  type t
    
  (** [create s] create and register a new logger named [s] *)
  val create: string -> t
  val incr: t -> unit
  val add: t -> int -> unit
  val print: Format.formatter -> t -> unit
end

(** timer logger *)
module Timer: sig
  type t
    
  (** [create s] create and register a new logger named [s] *)
  val create: string -> t
  val start: t -> unit
  val stop: t -> 'a -> 'a
  val print: Format.formatter -> t -> unit
end
