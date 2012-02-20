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

(** Input routines *)

open ExtLib

IFDEF HASZIP THEN
(** load a file in gzip format
    @return ExtLib.IO.input channel *)
val gzip_open_file : string -> IO.input
END

IFDEF HASBZ2 THEN
(** load a file in bzip format - Not implemented yet
    @return ExtLib.IO.input channel *)
val bzip_open_file : string -> IO.input
END

(** load a non compressed file  
    @return ExtLib.IO.input channel *)
val std_open_file : string -> IO.input

(** load a file either in gzip, bzip or not compressed format
    @return ExtLib.IO.input channel *)
val open_file : string -> IO.input

val open_ch : in_channel -> IO.input
val close_ch : IO.input -> unit

(** parse a uri.
    i.e. :
      deb://path/to/file
      rpm://path/to/file
      pgsql://test:tester@localhost/dbname?v1=k1&v2=k2
      sqlite://path/to/file
      cudf://path/to/file

    @return a tuple representing the uri *)
val parse_uri : string -> (
  Url.input_scheme *            (* format *)
  (string option      (* username *)
  * string option     (* password *)
  * string option     (* hostname *)
  * string option     (* port *)
  * string )          (* db name - or filename *)
  * string option     (* query string *)
  )

(** guess the input format from a list of list of uris and check
 *  if the list is omogenueous w.r.t the guessed format. Fails otherwise *)
val guess_format : string list list -> Url.input_scheme
