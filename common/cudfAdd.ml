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

let progressbar = Util.Progress.create "CudfAdd.build_maps"

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
    let equal = Cudf.(=%)
    let hash p = Hashtbl.hash (p.Cudf.package,p.Cudf.version)
  end)

(** specialized Set for cudf packages *)
module Cudf_set =
  OCAMLSet.Make(struct
    type t = Cudf.package
    let compare = Cudf.(<%) 
  end)

let to_set l = List.fold_right Cudf_set.add l Cudf_set.empty

(** additional functions on Cudf data type.  *)

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
  Format.fprintf fmt "%s (= %a)" pkg.Cudf.package pp_version pkg

let string_of_version = string_of pp_version
let string_of_package = string_of pp_package

let is_essential pkg =
  try (Cudf.lookup_package_property pkg "essential") = "yes"
  with Not_found -> false

(*
(** [pkgnames universe] returns a list of unique package names *)
let pkgnames universe =
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.fold_packages (fun acc pkg ->
    if not (Hashtbl.mem h pkg.Cudf.package) then begin
      Hashtbl.add h pkg.Cudf.package () ;
      pkg.Cudf.package::acc
    end else acc
  ) [] universe
*)

module StringSet = OCAMLSet.Make(String)

let pkgnames universe =
  Cudf.fold_packages (fun names pkg ->
    StringSet.add pkg.Cudf.package names
  ) StringSet.empty universe

(** maps one to one cudf packages to integers *)
class projection = object(self)

  val vartoint = Cudf_hashtbl.create 1023
  val inttovar = Util.IntHashtbl.create 1023
  val mutable counter = 0

  method init = List.iter self#add

  (** add a cudf package to the map *)
  method add (v : Cudf.package) =
    let j = counter in
    Cudf_hashtbl.add vartoint v j ;
    Util.IntHashtbl.add inttovar j v ;
    counter <- counter + 1

  (** var -> int *)
  method vartoint (v : Cudf.package) : int =
    try Cudf_hashtbl.find vartoint v
    with Not_found ->
      failwith (
        Printf.sprintf
        "cudfAdd - vartoint package %s does not exist"
        (string_of_package v)
      )
      
  (** int -> var *)
  method inttovar (i : int) : Cudf.package =
    try Util.IntHashtbl.find inttovar i 
    with Not_found ->
      failwith (
        Printf.sprintf
        "cudfAdd - inttovar var does not exist %d" i
      )
end 

(** build an hash table that associates (package name, String version) to
 * cudf packages *)
let realversionmap pkglist =
  let h = Hashtbl.create (5 * (List.length pkglist)) in
  List.iter (fun pkg ->
    Hashtbl.add h (pkg.Cudf.package,string_of_version pkg) pkg
  ) pkglist ;
  h

(*
 the Cudf library does not consider features of packages that are not
 installed. who_provides is a lookup function that returns all packages
 (installed or not) that implement a given feature 
*)
type maps = {
  (** the list of all packages the explicitely or implicitely
      conflict with the given package *)
  who_conflicts : Cudf.package -> Cudf.package list;

  (** [lookup_virtual feature] the list of all packages that
      explicitely declared [feature] as provide *)
  lookup_virtual : Cudf_types.vpkg -> Cudf.package list ;

  (** [who_provides constr] the list of all packages that satisfy [constr]. 
      A package is provided by real packages or other packages that provide that
      name as a feature. *)
  who_provides : Cudf_types.vpkg -> Cudf.package list ;

  (** assign an integer to each cudf package *)
  map : projection
}

(* constraints set *)
module CSet =
  OCAMLSet.Make(struct
    type t = (Cudf.package * Cudf_types.version option)
    let compare c1 c2 = 
      match c1,c2 with
      |(p1,None),(p2,None) when equal p1 p2 -> 0
      |(p1,Some v1),(p2,Some v2) when (equal p1 p2 && v1 = v2) -> 0
      |_ -> 1
  end)

(** build a map from a cudf universe *)
let build_maps universe =
  let size = Cudf.universe_size universe in
  let conflicts = Cudf_hashtbl.create (3 * size) in
  let provides = Util.StringHashtbl.create (3 * size) in
  let map = new projection in

  Util.Progress.set_total progressbar size ;

  let add_provides name constr =
    try let s = Util.StringHashtbl.find provides name in s := CSet.add constr !s 
    with Not_found -> Util.StringHashtbl.add provides name (ref (CSet.singleton constr))
  in
  let find_provides pkgname =
    try CSet.elements !(Util.StringHashtbl.find provides pkgname)
    with Not_found -> []
  in
  let add_conflict pkg1 pkg2 =
    try let s = Cudf_hashtbl.find conflicts pkg1 in s := Cudf_set.add pkg2 !s 
    with Not_found -> Cudf_hashtbl.add conflicts pkg1 (ref (Cudf_set.singleton pkg2))
  in
  let find_conflicts pkg =
    try Cudf_set.elements !(Cudf_hashtbl.find conflicts pkg)
    with Not_found -> []
  in

  Cudf.iter_packages (fun pkg ->
    Util.Progress.progress progressbar;
    (* associates an integer to each package *)
    map#add pkg; 
    (* associates to each feature the package providing it 
     * with a specific constraint *)
    List.iter (function
      |name, None when name = pkg.Cudf.package -> ()
      |name, None -> add_provides name (pkg, None)
      |name, Some (_, ver) -> add_provides name (pkg, (Some ver))
    ) pkg.Cudf.provides
  ) universe
  ;
  Util.Progress.reset progressbar;

  let lookup_virtual (pkgname,constr) =
    List.filter_map (function
      |pkg, None -> Some(pkg)
      |pkg, Some v when Cudf.version_matches v constr -> Some(pkg)
      |_,_ -> None
    ) (find_provides pkgname)
  in

  let who_provides (pkgname,constr) =
    match constr with
    |None -> 
        List.fold_left (fun acc (pkg,_) -> pkg::acc)
        (Cudf.lookup_packages universe pkgname)
        (find_provides pkgname)
    |Some _ ->
        List.fold_left (fun acc -> function
          |pkg, None -> pkg::acc
          |pkg, Some v when Cudf.version_matches v constr -> pkg::acc
          |_,_ -> acc
        )
        (Cudf.lookup_packages ~filter:constr universe pkgname)
        (find_provides pkgname)
  in

  (* we need to iterate twice on the package list as we need the
   * list of all virtual packages in order to generate the conflict list *)
  Cudf.iter_packages (fun pkg ->
    Util.Progress.progress progressbar;
    List.iter (fun (name,constr) ->
      List.iter (fun p ->
        if not(equal p pkg) then begin
          add_conflict pkg p ;
          add_conflict p pkg
        end
      ) (who_provides (name,constr))
    ) pkg.Cudf.conflicts
  ) universe
  ;
  Util.Progress.reset progressbar;

  {
    who_conflicts = find_conflicts;
    who_provides = who_provides;
    lookup_virtual = lookup_virtual ;
    map = map
  }
;;

let not_allowed = Str.regexp  "[^a-zA-Z0-9@/+().-]" 
let search s =
  try (Str.search_forward not_allowed s 0) >= 0
  with Not_found -> false

let encode s =
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

let cudfop = function
  |Some(("<<" | "<"),v) -> Some(`Lt,v)
  |Some((">>" | ">"),v) -> Some(`Gt,v)
  |Some("<=",v) -> Some(`Leq,v)
  |Some(">=",v) -> Some(`Geq,v)
  |Some("=",v) -> Some(`Eq,v)
  |Some("ALL",v) -> None
  |None -> None
  |Some(c,v) -> (Printf.eprintf "%s %s" c v ; assert false)
