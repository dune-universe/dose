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

(** Progress bar Timer and Logger *)

(** progress bar *)
module Progress : sig
  type label = string
  type t
    
  (** [create "barname"] : create new a progress bar labelled "barname". 
      The progress bar is disabled by default *)
  val create: label -> t

  (** [enable "barname"] : enable the progress bar with label "barname" *)
  val enable : label -> unit
  
  (** [set_total bar 10] : set the max width of the progress bar to 10 units *)
  val set_total : t -> int -> unit
  
  (** increment the progress bar of one unit *)
  val progress : t -> unit

  (** return the labels of all available progress bar *)
  val avalaible : unit -> label list
end

(** timer logger *)
module Timer : sig
  type t
    
  (** [create s] create and register a new logger named [s] *)
  val create: string -> t
  val start: t -> unit
  val stop: t -> 'a -> 'a
  val print: Format.formatter -> t -> unit
end

type verbosity = Quiet | Summary | Details
val set_verbosity: verbosity -> unit

val print_warning : ?ppf:Format.formatter -> ?v:verbosity -> string -> unit
val print_info : ?ppf:Format.formatter -> ?v:verbosity -> string -> unit

val gettimeofday: (unit -> float) ref

(** register a logger *)
val register: verbosity -> (Format.formatter -> unit) -> unit

(** dump the content of all registered loggers to the given formatter *)
val dump: Format.formatter -> unit

(** counter logger *)
module Counter : sig
  type t
    
  (** [create s] create and register a new logger named [s] *)
  val create: string -> t

  val incr: t -> unit
  val add: t -> int -> unit
  val print: Format.formatter -> t -> unit
end


