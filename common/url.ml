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

exception Invalid_url of string;;

include Util.Logging(struct let label = __FILE__ end) ;;

(***********************************************************************)
(* Input schemes *******************************************************)

type debtypes = [ `Edsp | `Deb ]
type rpmtypes = [ `Synthesis | `Hdlist ]
type osgitypes = [ `Eclipse ]
type othertypes = [ `Csw ]

type filetypes = [ `Cudf | debtypes | rpmtypes | osgitypes | othertypes ]
type datatypes = [ `Sqlite | `Pgsql ]
type input_scheme = [ filetypes | datatypes ]

let is_local_scheme = function
  | #filetypes | `Sqlite -> true
  | `Pgsql -> false
;;

let scheme_to_string = function
  | `Edsp -> "edsp"
  | `Csw -> "csw"
  | `Deb -> "deb"
  | `Eclipse -> "eclipse"
  | `Cudf -> "cudf"
  | `Synthesis -> "synthesis"
  | `Hdlist -> "hdlist"
  | `Pgsql -> "pgsql"
  | `Sqlite -> "sqlite"
;;

let scheme_of_string = function
  | "edsp" -> `Edsp
  | "csw" -> `Csw
  | "deb" -> `Deb
  | "cudf" -> `Cudf
  | "eclipse" -> `Eclipse
  | "synthesis" -> `Synthesis
  | "hdlist" -> `Hdlist
  | "sqlite" -> `Sqlite
  | "pgsql" -> `Pgsql
  | s -> fatal "unknown input scheme" s
;;

(***********************************************************************)

type url = {
  scheme : input_scheme;
  host   : string option;
  port   : string option;
  path   : string; (** db name or filename *)
  user   : string option;
  passwd : string option;
  query  : (string * string) list; (** query string *)
};;


(************************************************************************)
(* printing *************************************************************)

let to_string u =
  if is_local_scheme u.scheme
  then (scheme_to_string u.scheme)^"://"^u.path
  else
    (scheme_to_string u.scheme) ^ "://"  ^
      (match u.user with
	| None -> ""
	| Some user -> 
	  ( match u.passwd with
	    | None -> user^"@"
	    | Some passwd -> user^":"^passwd^"@")) ^
      (match u.host with
	| None -> ""
	| Some host -> host)^ 
      (match u.port with
	| None -> ""
	| Some port -> ":"^port)^
      "/" ^ u.path ^
      (match u.query with
	| [] -> ""
	| [(key,value)] -> "?"^key^"="^value
	| (firstkey,firstvalue)::rest ->
	  (List.fold_left
	     (fun acc (key,value) -> acc^";"^key^"="^value)
	     ("?"^firstkey^"="^firstvalue)
	     rest))
;;

(********************************************************************)
(* parsing **********************************************************)

(* parse a query from the string [s], starting from position [from] *)
(* until the end of [s]. [length] is the length of [s].             *)
(* pairs in the query are separated by ';'. In each pair, key and   *)
(* are separated by '='. A teminating ';' is accepted but not       *)
(* required                                                         *)
let rec parse_query_from s from length =
  if from >= length
  then []
  else
    try
      let pos_equal = String.index_from s from '=' in
      try
	let pos_semicolon = String.index_from s (pos_equal+1) ';' in
	((String.sub s from (pos_equal-from)),
	 (String.sub s (pos_equal+1) (pos_semicolon-pos_equal-1)))::
	  (parse_query_from s (pos_semicolon+1) length)
      with Not_found ->
	[(String.sub s from (pos_equal-from)),
	 (String.sub s (pos_equal+1) (length-pos_equal-1))]
    with
	Not_found -> fatal "no '=' found after position %d %s" from s

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
  if is_local_scheme scheme
  then
      (* we have a local file scheme: everything after the seperator *)
      (* constitutes the path name.                                  *)
      { scheme  = scheme;
	path    = String.sub s start_rest (l-start_rest);
	user    = None;
	passwd  = None;
	host    = None;
	port    = None;
	query   = []
      }
  else
      (* we have a data base scheme: split the rest *)
      let pos_slash =
	(* position of the first slash in the rest, which is mandatory  *)
        (* since it separates the authentication part from the path     *)
	try String.index_from s start_rest '/'
	with Not_found ->
	  fatal "remote scheme needs authentication/path separator" s
      in
      (* The first path segment in case of a remote scheme must not be  *)
      (* empty.                                                         *)
      if pos_slash=start_rest
      then fatal "path of a remote scheme must not start on '/'" s;
      let pos_at_in_auth =
	(* the position of @ before the first slash, or -1 otherwise    *)
	try 
	  let pos_at = String.index_from s start_rest '@' in
	  if pos_at < pos_slash then pos_at else -1
	with Not_found -> -1
      in
      let user,passwd =
	if pos_at_in_auth < 0 
	then (* [user[:pass]@] is missing *)
	  None,None
	else (* we have @ before the first / *)
	  try
	    let sep_user_pass = String.index_from s start_rest ':'
	    in
	    if sep_user_pass > pos_at_in_auth 
	    then (* ignore ":" that comes after "@": no password *)
	      Some (String.sub s start_rest (pos_at_in_auth-start_rest)),
	      None
	    else (* we have user and password *)
	      Some (String.sub s start_rest (sep_user_pass-start_rest)),
	      Some (String.sub s (sep_user_pass+1)
		      (pos_at_in_auth-sep_user_pass-1))
	  with Not_found ->
	    (* no ":" at all: we only have a user but no password *)
	    Some (String.sub s start_rest (pos_at_in_auth-start_rest)),
	    None
      and host,port =
	let start_host =
	  (* if the rest starts on user[:passwd]@ then the host[:port] *)
	  (* part starts right after the @, oherwise it starts at the  *)
	  (* beginning of the rest.                                    *)
	  if pos_at_in_auth<0 then start_rest else pos_at_in_auth+1
	in try
	     let sep_host_port = String.index_from s start_host ':'
	     in 
	     if sep_host_port > pos_slash 
	     then (* ignore ":" that comes after "/": no port *)
	       Some (String.sub s start_host (pos_slash-start_host)),
	       None
	     else (* we have a host and a port *)
	       Some (String.sub s start_host (sep_host_port-start_host)),
	       Some (String.sub s
		       (sep_host_port+1) (pos_slash-sep_host_port-1))
	  with Not_found ->
	    (* no ":" at all: only host, no port *)
	    Some (String.sub s start_host (pos_slash-start_host)),
	    None
      and path,query =
	try
	  let sep_path_query = String.index_from s (pos_slash+1) '?'
	  in (* we have path and query *)
	  (String.sub s (pos_slash+1) (sep_path_query-pos_slash-1)),
	  (parse_query_from s (sep_path_query+1)) l
	with Not_found ->
	  (* no "?": only path, no query *)
	  (String.sub s (pos_slash+1) (l-pos_slash-1)),
	  []
      in
      { scheme = scheme;
	path   = path;
	user   = user;
	passwd = passwd;
	host   = host;
	port   = port;
	query  = query
      }
;;
