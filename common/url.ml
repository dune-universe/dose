(****************************************************************************)
(*  Copyright (C) 2011,2012,2013 Ralf Treinen                               *)
(*                       <ralf.treinen@pps.univ-paris-diderot.fr>           *)
(*                                                                          *)
(*  This library is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU Lesser General Public License as          *)
(*  published by the Free Software Foundation, either version 3 of the      *)
(*  License, or (at your option) any later version.  A special linking      *)
(*  exception to the GNU Lesser General Public License applies to this      *)
(*  library, see the COPYING file for more information.                     *)
(****************************************************************************)

exception Invalid_url of string;;

include Util.Logging(struct let label = __FILE__ end) ;;

(***********************************************************************)
(* Input schemes *******************************************************)

type debtypes = [ `Edsp | `Deb ]
type rpmtypes = [ `Synthesis | `Hdlist ]
type osgitypes = [ `Eclipse ]
type othertypes = [ `Csw | `Cv ]

type filetypes = [ `Cudf | debtypes | rpmtypes | osgitypes | othertypes ]

let scheme_to_string = function
  | `Edsp -> "edsp"
  | `Csw -> "csw"
  | `Cv -> "cv"
  | `Deb -> "deb"
  | `Eclipse -> "eclipse"
  | `Cudf -> "cudf"
  | `Synthesis -> "synthesis"
  | `Hdlist -> "hdlist"
;;

let scheme_of_string = function
  | "edsp" -> `Edsp
  | "csw" -> `Csw
  | "cv" -> `Cv
  | "deb" -> `Deb
  | "cudf" -> `Cudf
  | "eclipse" -> `Eclipse
  | "synthesis" -> `Synthesis
  | "hdlist" -> `Hdlist
  | s -> fatal "unknown input scheme" s
;;

let supported_input_types =
  [`Edsp; `Deb ; `Synthesis ; `Hdlist ; `Eclipse ; `Csw ; `Cudf ; `Cv ]
;;

(***********************************************************************)
(* URLs ****************************************************************)

type url = {
  scheme : filetypes;
  path   : string;     (* filename *)
};;

let to_string u = (scheme_to_string u.scheme)^"://"^u.path
;;

let of_string s =
  let l = String.length s in
  let pos_colon =
    try String.index s ':'
    with Not_found -> fatal "missing '://' separator %s" s
  in
  if pos_colon+2 >= l
    || String.get s (pos_colon+1) <> '/'
    || String.get s (pos_colon+2) <> '/'
  then fatal "missing '://' separator %s" s;
  let scheme = scheme_of_string (String.sub s 0 pos_colon)
  and start_rest = pos_colon+3 in
  { scheme  = scheme;
    path    = String.sub s start_rest (l-start_rest);
  }
;;
