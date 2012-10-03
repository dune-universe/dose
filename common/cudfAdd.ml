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


(* Remember the original hashtable module from Ocaml standard library,
    whose name will be overriden by opening Extlib. *)
module OCAMLHashtbl = Hashtbl

open ExtLib

(* Include internal debugging functions for this module (debug, info, warning, fatal). *)
include Util.Logging(struct let label = __FILE__ end) ;;

let equal = Cudf.(=%)
let compare = Cudf.(<%)

let hash p = Hashtbl.hash (p.Cudf.package,p.Cudf.version)

module Cudf_hashtbl =
  OCAMLHashtbl.Make(struct
    type t = Cudf.package
    let equal = equal
    let hash = hash
  end)

module Cudf_set =
  Set.Make(struct
    type t = Cudf.package
    let compare = compare
  end)

let to_set l = List.fold_right Cudf_set.add l Cudf_set.empty


(* encode - decode *)

(* Specialized hashtable for encoding strings efficiently. *)
module EncodingHashtable =
  OCAMLHashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = fun s -> Char.code (s.[0])
  end)

(* Specialized hashtable for decoding strings efficiently. *)
module DecodingHashtable =
  OCAMLHashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = (fun s -> (Char.code s.[1]) * 1000 + (Char.code s.[2]) )
  end)


(*  "hex_char char" returns the ASCII code of the given character
    in the hexadecimal form, prefixed with the '%' sign.
    e.g. hex_char '+' = "%2b" *)
let hex_char char = Printf.sprintf "%%%02x" (Char.code char);;

(* "init_hashtables" initializes the two given hashtables to contain:

    - enc_ht: Precomputed results of applying the function "hex_char"
    to all possible ASCII chars.
    e.g. EncodingHashtable.find enc_ht "+" = "%2b"

    - dec_ht: An inversion of enc_ht.
    e.g. DecodingHashtable.find dec_ht "%2b" = "+" 
*)
let init_hashtables enc_ht dec_ht =

  (* "all_ascii_chars" is a list containing characters with 
     all possible ASCII codes (i.e. between 0 and 255). *)
  let all_ascii_chars = 
    (*  "all_numbers_from_to first last" generates a list of
	all integers between the first one and the last one
	(including both ends). *)
    let rec all_numbers_from_to first last =
      if first <= last
      then first :: (all_numbers_from_to (first + 1) last)
      else []
    in
    (* Convert each number from 0 to 255 to a character with the given ASCII code. *)
    List.map Char.chr (all_numbers_from_to 0 255)
      
  in
  (* One by one add all the possible pairs (char, hexed_char)
      to both hashtables. *)
  List.iter (fun char -> 
    let char_as_string = String.make 1 char in
    let hexed_char = hex_char char in
    EncodingHashtable.add enc_ht char_as_string hexed_char;
    DecodingHashtable.add dec_ht hexed_char char_as_string
      ) all_ascii_chars
    ;;

(* Create and initialize twin hashtables,
   one for encoding and one for decoding. *)
let enc_ht = EncodingHashtable.create 256;;
let dec_ht = DecodingHashtable.create 256;;
init_hashtables enc_ht dec_ht;;

(* encode *)
let encode_single s   = EncodingHashtable.find enc_ht s;;
let not_allowed_regexp = Pcre.regexp "[^a-zA-Z0-9@/+().-]";;

let encode s =
  Pcre.substitute ~rex:not_allowed_regexp ~subst:encode_single s
;;

(* decode *)
let decode_single s = DecodingHashtable.find dec_ht s;;
let encoded_char_regexp = Pcre.regexp "%[0-9a-f][0-9a-f]";;

let decode s =
  Pcre.substitute ~rex:encoded_char_regexp ~subst:decode_single s
;;

(* formatting *)

let string_of pp arg =
  ignore(pp Format.str_formatter arg);
  Format.flush_str_formatter ()

let pp_version fmt pkg =
  try Format.fprintf fmt "%s" (decode (Cudf.lookup_package_property pkg "number"))
  with Not_found -> Format.fprintf fmt "%d" pkg.Cudf.version

let pp_package fmt pkg =
  Format.fprintf fmt "%s (= %a)" (decode pkg.Cudf.package) pp_version pkg

let string_of_version = string_of pp_version
let string_of_package = string_of pp_package

module StringSet = Set.Make(String)

let pkgnames universe =
  Cudf.fold_packages (fun names pkg ->
    StringSet.add pkg.Cudf.package names
  ) StringSet.empty universe


let add_properties preamble l =
  List.fold_left (fun pre prop ->
    {pre with Cudf.property = prop :: pre.Cudf.property }
  ) preamble l

let is_essential pkg =
  try (Cudf.lookup_package_property pkg "essential") = "yes"
  with Not_found -> false

let realversionmap pkglist =
  let h = Hashtbl.create (5 * (List.length pkglist)) in
  List.iter (fun pkg ->
    Hashtbl.add h (pkg.Cudf.package,string_of_version pkg) pkg
  ) pkglist ;
  h

let vartoint = Cudf.uid_by_package 
let inttovar = Cudf.package_by_uid

let add_to_package_list h n p =
  try let l = Hashtbl.find h n in l := p :: !l
  with Not_found -> Hashtbl.add h n (ref [p])

let get_package_list h n = try !(Hashtbl.find h n) with Not_found -> []

let unique l = 
  List.rev (List.fold_left (fun results x -> 
    if List.mem x results then results 
    else x::results) [] l
  )
;;

let normalize_set (l : int list) = unique l

(* (pkgname,constr) -> pkg *)
let who_provides univ (pkgname,constr) = 
  let pkgl = Cudf.lookup_packages ~filter:constr univ pkgname in
  let prol = Cudf.who_provides ~installed:false univ (pkgname,constr) in
  pkgl @ (List.map fst prol)

(* vpkg -> id list *)
let resolve_vpkg_int univ vpkg =
  List.map (Cudf.uid_by_package univ) (who_provides univ vpkg)

(* vpkg list -> id list *)
let resolve_vpkgs_int univ vpkgs =
  normalize_set (List.flatten (List.map (resolve_vpkg_int univ) vpkgs))

(* vpkg list -> pkg list *)
let resolve_deps univ vpkgs =
  List.map (Cudf.package_by_uid univ) (resolve_vpkgs_int univ vpkgs)

(* pkg -> pkg list list *)
let who_depends univ pkg = 
  List.map (resolve_deps univ) pkg.Cudf.depends

let who_conflicts conflicts_packages univ pkg = 
  if (Hashtbl.length conflicts_packages) = 0 then
    warning "Either there are no conflicting packages in the universe or you
CudfAdd.init_conflicts was not invoked before calling CudfAdd.who_conflicts";
  let i = Cudf.uid_by_package univ pkg in
  List.map (Cudf.package_by_uid univ) (get_package_list conflicts_packages i)
;;

let init_conflicts univ =
  let conflict_pairs = Hashtbl.create 1023 in
  let conflicts_packages = Hashtbl.create 1023 in
  Cudf.iteri_packages (fun i p ->
    List.iter (fun n ->
      let pair = (min n i, max n i) in
      if n <> i && not (Hashtbl.mem conflict_pairs pair) then begin
        Hashtbl.add conflict_pairs pair ();
        add_to_package_list conflicts_packages i n;
        add_to_package_list conflicts_packages n i
      end
    )
    (resolve_vpkgs_int univ p.Cudf.conflicts)
  ) univ;
  conflicts_packages
;;

(* here we assume that the id given by cudf is a sequential and dense *)
let compute_pool universe = 
  let size = Cudf.universe_size universe in
  let conflicts = init_conflicts universe in
  let c = Array.init size (fun i -> get_package_list conflicts i) in
  let d =
    Array.init size (fun i ->
      let p = inttovar universe i in
      List.map (resolve_vpkgs_int universe) p.Cudf.depends
    )
  in
  (d,c)
;;

let cudfop = function
  |Some(("<<" | "<"),v) -> Some(`Lt,v)
  |Some((">>" | ">"),v) -> Some(`Gt,v)
  |Some("<=",v) -> Some(`Leq,v)
  |Some(">=",v) -> Some(`Geq,v)
  |Some("=",v) -> Some(`Eq,v)
  |Some("ALL",v) -> None
  |None -> None
  |Some(c,v) -> fatal "%s %s" c v

let latest pkglist =
  let h = Hashtbl.create (List.length pkglist) in
  List.iter (fun p ->
    try
      let q = Hashtbl.find h p.Cudf.package in
      if (compare p q) > 0 then
        Hashtbl.replace h p.Cudf.package p
      else ()
    with Not_found -> Hashtbl.add h p.Cudf.package p
  ) pkglist;
  Hashtbl.fold (fun _ v acc -> v::acc) h []
;;

let pp from_cudf ?(decode=decode) pkg =
  let (p,i) = (pkg.Cudf.package,pkg.Cudf.version) in
  let v = if i > 0 then snd(from_cudf (p,i)) else "nan" in
  let l =
    List.filter_map (fun k ->
      try Some(k,decode(Cudf.lookup_package_property pkg k))
      with Not_found -> None
    ) ["architecture";"source";"sourcenumber";"essential"]
  in (decode p,decode v,l)
;;
