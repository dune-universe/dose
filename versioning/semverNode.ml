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

(** this functions follow the semantic versioning specification http://semver.org/ 
 * and node's https://docs.npmjs.com/misc/semver.
 * *)

module Pcre = Re_pcre

open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

(* ************************ *)
let number_identifier_str = "0|[1-9]\\d*"

let number_identifier_loose_str = "[0-9]+"

let non_number_identifier_str = "\\d*[a-zA-Z-][a-zA-Z0-9-]*"

let main_version_builder ident =
  Printf.sprintf "(%s)\\.(%s)\\.(%s)" ident ident ident

let main_version_str =
  main_version_builder number_identifier_str

let main_version_loose_str =
  main_version_builder number_identifier_loose_str

let pre_release_identifier_builder number_ident non_number_ident =
  Printf.sprintf "(?:%s|%s)" number_ident non_number_ident

let pre_release_identifier_str =
  pre_release_identifier_builder number_identifier_str non_number_identifier_str

let pre_release_identifier_loose_str =
  pre_release_identifier_builder number_identifier_loose_str non_number_identifier_str

let pre_release_builder pre_release_ident =
  Printf.sprintf "(?:-(%s(?:\\.%s)*))" pre_release_ident pre_release_ident

let pre_release_str =
  pre_release_builder pre_release_identifier_str

let pre_release_loose_str =
  pre_release_builder pre_release_identifier_loose_str

let build_identifier_str = "[0-9A-Za-z-]+"

let build_str =
  Printf.sprintf "(?:\\+(%s(?:\\.%s)*))" build_identifier_str build_identifier_str

let full_plain_str =
  Printf.sprintf "v?%s%s?%s?" main_version_str pre_release_str build_str

let full_str = Printf.sprintf "^%s$" full_plain_str

let loose_plain_str =
  Printf.sprintf "[v=\\s]*%s%s?%s?" main_version_loose_str pre_release_loose_str build_str

let loose_str = Printf.sprintf "^%s$" loose_plain_str
(* ********************************* *)

let loose_re = Pcre.regexp loose_str
let full_re = Pcre.regexp full_str
let sep_re = Pcre.regexp "\\." 

type identifier =
  | NumericIdentifier of int
  | StringIdentifier of string

type version = {
  major: int;
  minor: int;
  patch: int;
  pre: identifier list;
  build: string list;
}

type version_expr =
  | And of (version_expr * version_expr)
  | Or of (version_expr * version_expr)
  | Version of string
  | Gte of version_expr
  | Lte of version_expr
  | Gt of version_expr
  | Lt of version_expr
  | Star

let print_version v =
  let pre =
    match v.pre with
    | [] -> [StringIdentifier "None"]
    | x -> x
  in
  let pre_to_string p =
    match p with
    | StringIdentifier s -> "Str: " ^ s
    | NumericIdentifier n -> "Num: " ^ (string_of_int n)
  in
  let pre_str = List.fold_left (fun acc x -> Printf.sprintf "%s %s" acc (pre_to_string x)) "" pre in
  Printf.printf "major = %d; minor = %d; patch = %d; pre = %s" v.major v.minor v.patch pre_str

let parse_version full version =
  let trimmed = String.trim version in
  let rex = if full then full_re else loose_re in
  try
    let parsed = Pcre.extract rex trimmed in
    let pre_identified =
      List.map (fun v ->
        try NumericIdentifier (int_of_string v)
        with Failure _ -> StringIdentifier v
      ) (Pcre.split sep_re parsed.(4))
    in
    { major = int_of_string parsed.(1);
      minor = int_of_string parsed.(2);
      patch = int_of_string parsed.(3);
      pre   = pre_identified;
      build = Pcre.split sep_re parsed.(5)
    }
  with Not_found ->
    raise (Invalid_argument "Invalid version")

let parse_version_option full version =
  try
    Some (parse_version full version)
  with
    Invalid_argument _ -> None

(*Compare  two elements of the prerelease part of a version*)
let compare_identifiers = function
  | (NumericIdentifier x1, NumericIdentifier y1) -> Pervasives.compare x1 y1
  | (StringIdentifier _, NumericIdentifier _)    -> 1
  | (NumericIdentifier _, StringIdentifier _)    -> -1
  | (StringIdentifier s1, StringIdentifier s2)   -> String.compare s1 s2

(* 1. Not having a prerelease is > that having one
   2. We compare each pre-release, the one with with less elements win or the
      one with a hight element.  *)
let compare_pre (l1,l2) =
  let lenl1 = List.length l1 in
  let lenl2 = List.length l2 in
  if lenl1 = 0 && lenl2 = 0 then 0
  else if lenl1 <> 0 && lenl2 = 0 then -1
  else if lenl1 = 0 && lenl2 <> 0 then 1
  else if lenl1 = 0 && lenl2 = 0 then 0
  else
    let rec check acc = function
      |[],[] -> acc
      |(x1::l1,x2::[]) when l1 <> [] -> 1
      |(x1::[],x2::l2) when l2 <> [] -> -1
      |(x1::l1,x2::l2) when x1 = x2 -> check 0 (l1,l2) 
      |(x1::_,x2::_) -> compare_identifiers (x1,x2)
      |_,_ -> assert false
    in check 0 (l1,l2)

let compare_version x y =
  let res x = if x = 0 then 0 else if x < 0 then -1 else 1 in
  let c1 = Pervasives.compare x.major y.major in
  if c1 <> 0 then res c1
  else
    let c2 = Pervasives.compare x.minor y.minor in
    if c2 <> 0 then res c2
  else
    let c3 = Pervasives.compare x.patch y.patch in
    if c3 <> 0 then res c3
  else
    compare_pre (x.pre,y.pre)

let parse_and_compare full x y =
  if x = y then 0 else
    let v1 = parse_version full x in
    let v2 = parse_version full y in
    compare_version v1 v2

let compare full x y = parse_and_compare full x y
let equal full x y = (compare full x y) = 0
