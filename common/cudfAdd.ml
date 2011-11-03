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

module OCAMLHashtbl = Hashtbl
module OCAMLSet = Set

open ExtLib

let fatal fmt = Util.make_fatal __FILE__ fmt

(* the id of a package *)
let id pkg = (pkg.Cudf.package,pkg.Cudf.version)

(** compare two cudf packages only using name and version *)
let compare = Cudf.(<%)

(** has a cudf package only using name and version *)
let hash p = Hashtbl.hash (p.Cudf.package,p.Cudf.version)

(** two cudf packages are equal if name and version are the same *)
let equal = Cudf.(=%)

(** specialized Hash table for cudf packages *)
module Cudf_hashtbl =
  OCAMLHashtbl.Make(struct
    type t = Cudf.package
    let equal = equal
    let hash = hash
  end)

(** specialized Set for cudf packages *)
module Cudf_set =
  OCAMLSet.Make(struct
    type t = Cudf.package
    let compare = compare
  end)

let to_set l = List.fold_right Cudf_set.add l Cudf_set.empty

(** additional functions on Cudf data type.  *)

let encode s =
  let not_allowed = Str.regexp  "[^a-zA-Z0-9@/+().-]" in
  let search s =
    try (Str.search_forward not_allowed s 0) >= 0
    with Not_found -> false
  in
  let make_hex chr = Printf.sprintf "%%%x" (Char.code chr) in
  if search s then begin
    let n = String.length s in
    let b = Buffer.create n in
    for i = 0 to n-1 do
      let s' = String.of_char s.[i] in
      if Str.string_match not_allowed s' 0 then
        Buffer.add_string b (make_hex s.[i])
      else
        Buffer.add_string b s'
    done;
    Buffer.contents b
  end else s

let decode s =
  let hex_re = Str.regexp "%[0-9a-f][0-9a-f]" in
  let un s =
    let s = Str.matched_string s in
    let hex = String.sub s 1 2 in
    let n = int_of_string ("0x" ^ hex) in
    String.make 1 (Char.chr n)
  in
  Str.global_substitute hex_re un s

let add_properties preamble l =
  List.fold_left (fun pre prop ->
    {pre with Cudf.property = prop :: pre.Cudf.property }
  ) preamble l

let buf = Buffer.create 1024
let buf_formatter =
  let fmt = Format.formatter_of_buffer buf in
    Format.pp_set_margin fmt max_int;
    fmt

let string_of pp arg =
  Buffer.clear buf;
  ignore(pp buf_formatter arg);
  Format.pp_print_flush buf_formatter ();
  Buffer.contents buf

let pp_version fmt pkg =
  try Format.fprintf fmt "%s" (Cudf.lookup_package_property pkg "number")
  with Not_found -> Format.fprintf fmt "%d" pkg.Cudf.version

let pp_package fmt pkg =
  Format.fprintf fmt "%s (= %a)" (decode pkg.Cudf.package) pp_version pkg

let string_of_version = string_of pp_version
let string_of_package = string_of pp_package

let is_essential pkg =
  try (Cudf.lookup_package_property pkg "essential") = "yes"
  with Not_found -> false

module StringSet = OCAMLSet.Make(String)

let pkgnames universe =
  Cudf.fold_packages (fun names pkg ->
    StringSet.add pkg.Cudf.package names
  ) StringSet.empty universe

(** build an hash table that associates (package name, String version) to
 * cudf packages *)
let realversionmap pkglist =
  let h = Hashtbl.create (5 * (List.length pkglist)) in
  List.iter (fun pkg ->
    Hashtbl.add h (pkg.Cudf.package,string_of_version pkg) pkg
  ) pkglist ;
  h

let vartoint = Cudf.uid_by_package 
let inttovar = Cudf.package_by_uid

let pkgid p = (p.Cudf.package, p.Cudf.version)

let add_to_package_list h n p =
  try let l = Hashtbl.find h n in l := p :: !l
  with Not_found -> Hashtbl.add h n (ref [p])

let get_package_list h n = try !(Hashtbl.find h n) with Not_found -> []

let normalize_set (l : int list) = Util.list_unique l

let who_provides univ (pkgname,constr) = 
  let prol = Cudf.who_provides ~installed:false univ (pkgname,constr) in
  let pkgl = Cudf.lookup_packages ~filter:constr univ pkgname in
  pkgl @ (List.map fst prol)

let resolve_package_dep univ (n, c) =
  List.map (Cudf.uid_by_package univ) (who_provides univ (n,c))

let resolve_deps_int univ vpkgs =
  normalize_set (List.flatten (List.map (resolve_package_dep univ) vpkgs))

let resolve_deps univ vpkgs =
  List.map (Cudf.package_by_uid univ) (resolve_deps_int univ vpkgs)

let who_depends univ pkg =
  List.map (resolve_deps univ) pkg.Cudf.depends

let who_conflicts conflicts_packages univ pkg = 
  if (Hashtbl.length conflicts_packages) = 0 then
    fatal "you must use CudfAdd.init_conflicts before using who_conflicts";
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
    (resolve_deps_int univ p.Cudf.conflicts)
  ) univ;
  conflicts_packages
;;

let compute_pool universe = 
  let size = Cudf.universe_size universe in
  let conflicts = init_conflicts universe in
  let c = Array.init size (fun i -> get_package_list conflicts i) in
  let d =
    Array.init size (fun i ->
      let p = inttovar universe i in
      List.map (resolve_deps_int universe) p.Cudf.depends
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
  |Some(c,v) -> (Printf.eprintf "%s %s" c v ; assert false)
