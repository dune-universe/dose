(*****************************************************************************)
(*  Copyright (C) 2009  <pietro.abate@pps.jussieu.fr>                        *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(** Input routines *)

open ExtLib

#ifdef HASZIP
(** load a file in gzip format
    @return ExtLib.IO.input channel *)
val gzip_open_in : string -> IO.input
#endif

#ifdef HASBZ2
(** load a file in bzip format - Not implemented yet
    @return ExtLib.IO.input channel *)
val bzip_open_in : string -> IO.input
#endif

(** load a non compressed file  
    @return ExtLib.IO.input channel *)
val std_open_in : string -> IO.input

(** load a file either in gzip, bzip or not compressed format
    @return ExtLib.IO.input channel *)
val open_chan : string -> IO.input

(** parse a uri.
    i.e. :
      deb://path/to/file
      rpm://path/to/file
      pgsql://abate:tester@localhost/dbname?v1=k1&v2=k2
      sqlite://path/to/file
      cudf://path/to/file

    @return a tuple representing the uri *)
val parse_uri : string -> (
  string *            (* format *)
  (string option      (* username *)
  * string option     (* password *)
  * string option     (* hostname *)
  * string option     (* port *)
  * string )          (* db name - or filename *)
  * string option     (* query string *)
  )
