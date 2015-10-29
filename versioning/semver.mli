(**************************************************************************************)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.univ-paris-diderot.fr>     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)
 
(** this functions follow the semantic versioning specification http://semver.org/ *)

(** [parse s] returns a tuple of the form (major,minor,patch,rest) *)
val parse : string -> string * string * string * string

(** compare two strings intepreted as semantic versions *)
val compare : string -> string -> int

val equal : string -> string -> bool
