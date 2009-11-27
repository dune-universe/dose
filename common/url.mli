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

(*
* deb://path/to/file
* rpm://path/to/file
* pgsql://abate:tester@localhost/dbname?v1=k1&v2=k2
* sqlite://path/to/file
* cudf://path/to/file
* *)

type url = {
  proto : string;
  server : string;
  port : int;
  full_file : string; (** db name - or filename including query *)
  file : string; (** db name - or filename *)
  user : string;
  passwd : string;
  args : (string * string) list; (* query string *)
  string : string; (** the input url *)
}

val encode : string -> string
val decode : string -> string

val of_string : ?args:(string * string) list -> string -> url
val to_string : url -> string
val to_string_no_args : url -> string

