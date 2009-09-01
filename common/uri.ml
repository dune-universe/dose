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

(*
deb://path/to/file
rpm://path/to/file
pgsql://abate:tester@localhost/dbname?v1=k1&v2=k2
sqlite://path/to/file
cudf://path/to/file
*)

exception Invalid_argument of string

let protocol_re = Str.regexp "^[a-zA-Z]+://"
let getProtocolSlashSlash s =
  if Str.string_match protocol_re s 0
  then
    let matched = Str.matched_string s in
    let len = String.length matched in
    let remainder = Str.string_after s len in
    let protocolName = String.sub matched 0 (len-3) in
    protocolName,remainder
  else
    raise(Invalid_argument (Printf.sprintf "ill-formed uri %s " s))
;;

let user_pass_re = Str.regexp "^\\([-_a-zA-Z0-9]+\\):\\([-_a-zA-Z0-9]+\\)@"
let user_re = Str.regexp "^[-_a-zA-Z0-9]+@"
let getUser s =
    if Str.string_match user_pass_re s 0 then
      let user = Str.matched_group 1 s in
      let pass = Str.matched_group 2 s in
      let len = String.length (Str.matched_string s) in
      let reminder = Str.string_after s len in
      (Some user, Some pass, reminder)
    else if Str.string_match user_pass_re s 0 then
      let userAt = Str.matched_string s in
      let len = String.length userAt in
      let reminder = Str.string_after s len in
      let user = String.sub userAt 0 (len-1) in
      (Some user, None, reminder)
    else (None,None,s)
;;

let hostRegexp = Str.regexp "^[-_a-zA-Z0-9.]+"
let getHost s =
  if Str.string_match hostRegexp s 0
  then
    let host = Str.matched_string s in
    let s' = Str.string_after s (String.length host) in
    (Some host,s')
  else (None,s)
;;

let colonPortRegexp = Str.regexp "^:[0-9]+"
let getPort s =
  if Str.string_match colonPortRegexp s 0
  then
    let colonPort = Str.matched_string s in
    let len = String.length colonPort in
    let port = (String.sub colonPort 1 (len-1)) in
    let s' = Str.string_after s len in
    (Some port,s')
  else (None,s)
;;

let pathRegexp = Str.regexp "^[^?]+"
let getPath s =
  if Str.string_match pathRegexp s 0
  then
    let path = Str.matched_string s in
    let len = String.length path in
    let reminder = Str.string_after s len in
    (Some path,reminder)
  else (None,s)
;;

let parseUri s =
  match getProtocolSlashSlash s with
  |(("deb"|"hdlist"|"synth"|"sqlite"|"cudf") as protocol,file) ->
      (protocol,(None,None,None,None,file),None)
  |("pgsql" as protocol,s0) ->
      let (userOpt,passOpt,s1) = getUser s0 in
      let (hostOpt,s2)   = getHost s1 in
      let (portOpt,s3)   = getPort s2 in
      let (pathOpt,_)    = getPath s3 in
      begin match pathOpt with
      |None -> raise(Invalid_argument (Printf.sprintf "ill-formed uri %s " s))
      |Some path -> (protocol,(userOpt,passOpt,hostOpt,portOpt,path),None)
      end
  |_ -> raise(Invalid_argument (Printf.sprintf "ill-formed uri %s " s))
;;

let print_conn (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
  let f = function
    |Some s -> s
    |None -> ""
  in
  Printf.printf "protocol:%s\nuser:%s\npass:%s\nhost:%s\nport:%s\npath:%s\n"
  protocol (f userOpt) (f passOpt) (f hostOpt) (f portOpt) (f path)
;;
