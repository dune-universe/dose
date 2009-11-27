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

open ExtLib

(* internal database representation *)

type distribution = [
    `Debian
]

type version_id = int
type name = string
type source = string
type number = string
type selector = Leq | Geq | Lt | Gt | Eq | Any

type package_selector = (name * string * number)

type cnf_relation = [
    | `Depends
    | `Pre_depends
    | `Recommends
    | `Suggests
    | `Enhances
]

type conj_relation = [
    | `Conflicts
    | `Breaks
    | `Provides
    | `Replaces
]

type relation = [ cnf_relation | conj_relation ]
type cnf_dep = (cnf_relation * package_selector list list)
type conj_dep = (conj_relation * package_selector list)

type package = {
    name : string ;
    number : number ;
    version_id : version_id;
    cnf_deps : cnf_dep list;
    conj_deps : conj_dep list;
    extra : (string * string) list
}

let parse_selector = function
    |"<=" -> Leq
    |">=" -> Geq
    |"<<" -> Lt
    |">>" -> Gt
    |"="  -> Eq
    |""   -> Any
    |s -> print_endline s ; assert false

type query = [
    |`And of query list
    |`Or of query list
    |`All

    |`Suite of string
    |`Comp of string
    |`Arch of string
    |`Date of string
    |`Interval of (string * string)

    |`Section of string
    |`Essential of bool
    |`Priority of string

    |`Version of ( string * string )
    |`Version_id of int
    |`Name of string
    |`Number of string
]

(************************************************************)

let space_re = Str.regexp "[ \t]+"
let and_sep_re = Str.regexp "\\s*,\\s*"
let or_sep_re = Str.regexp "\\s*|\\s*"
let pkg_re = Str.regexp "(\\([a-zA-Z][a-zA-Z\\-+_]+\\)\\s*,\\s*\\([0-9][0-9]+\\))"
let comm_re = Str.regexp "^#.*$"
let eq_sep_re = Str.regexp "\\s*=\\s*"

let parse_aptlist s =
  let query_sep_RE = Str.regexp "\\s*\\/\\s*" in
  let l = List.mapi (fun i e -> match i with
      |0 -> `Arch e
      |1 -> `Suite e
      |2 -> `Comp e
      |3 -> `Date e
      |_ -> Printf.eprintf "parsing error %s\n" s ; exit (-1)
    ) (Str.split query_sep_RE s)
  in `And l

let parse_cnf p s =
  let and_args = Str.split and_sep_re s in
    List.map (fun and_arg ->
      let or_args = Str.split or_sep_re and_arg in
      List.map p or_args
    ) and_args

(* parse a string query *)
let parse_query s =
  let parse_aux str =
    let res =
      match Str.split space_re str with
      |["Date";d;t] -> `Date (Printf.sprintf "%s %s" d t)
      |["Interval";d1;d2] -> `Interval (d1,d2)
      |["Suite";ss] -> `Suite ss
      |["Priority";ss] -> `Priority ss
      |["Section";ss] -> `Section ss
      |["Essential";"true"] -> `Essential true
      |["Essential";"false"] -> `Essential false
      |_ -> (Printf.eprintf "Parse error %s\n" s ; exit (-1))
    in (res :> query)
  in
  `And (List.map (fun disj -> `Or disj ) (parse_cnf parse_aux s))
;;
