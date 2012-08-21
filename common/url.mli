(****************************************************************************)
(*  Copyright (C) 2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>           *)
(*  Copyright (C) 2011 Mancoosi Project                                     *)
(*                                                                          *)
(*  This library is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU Lesser General Public License as          *)
(*  published by the Free Software Foundation, either version 3 of the      *)
(*  License, or (at your option) any later version.  A special linking      *)
(*  exception to the GNU Lesser General Public License applies to this      *)
(*  library, see the COPYING file for more information.                     *)
(****************************************************************************)

type input_scheme =
  | Edsp | Csw | Deb | Cudf | Eclipse | Synthesis | Hdlist  (* file types *)
  | Sqlite | Pgsql                             (* data bases *)
;;

type url = {
  scheme : input_scheme;
  host   : string option;
  port   : string option;
  path   : string; (** db name or filename *)
  user   : string option;
  passwd : string option;
  query  : (string * string) list; (** query string *)
};;

(* parsing of a string as url. Raises Invalid_url in case of error *)
val of_string: string -> url
exception Invalid_url of string;;

val to_string: url -> string
val scheme_to_string: input_scheme -> string
val scheme_of_string: string -> input_scheme
