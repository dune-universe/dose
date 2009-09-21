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

open Cudf
open Cudf_types

let print_package ?(short=true) pkg =
  if short then
    let (sp,sv) =
      try (pkg.package,Cudf.lookup_package_property pkg "Number")
      with Not_found -> (pkg.package,string_of_int pkg.version)
    in Printf.sprintf "%s (= %s)" sp sv
  else
    Cudf_printer.string_of_package pkg

(* I want to hash packages by name/version without considering
   other fields like Installed / keep / etc.
*)
let compare x y =
  Pervasives.compare
  (x.Cudf.package,x.Cudf.version)
  (y.Cudf.package,y.Cudf.version)
let hash p = Hashtbl.hash (p.Cudf.package,p.Cudf.version)
let equal = Cudf.(=%)


module Cudf_hashtbl =
  Hashtbl.Make(struct
    type t = Cudf.package
    let equal = equal
    let hash = hash
  end)

module Cudf_set =
  Set.Make(struct
    type t = Cudf.package
    let compare = compare
  end)

open ExtLib

let parse_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    Cudf_parser.parse p
  with
  |Cudf_parser.Parse_error _
  | Cudf.Constraint_violation _ as exn -> begin
    Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
    doc (Printexc.to_string exn);
    exit 1
  end

let load_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    Cudf_parser.load p
  with
  |Cudf_parser.Parse_error _
  | Cudf.Constraint_violation _ as exn -> begin
    Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
    doc (Printexc.to_string exn);
    exit 1
  end

(* maps one to one cudf packages to integers *)
class projection = object(self)

  val vartoint = Cudf_hashtbl.create 1023
  val inttovar = Hashtbl.create 1023
  val mutable counter = 0

  method init = List.iter self#add

  method add (v : Cudf.package) =
    let j = counter in
    Cudf_hashtbl.add vartoint v j ;
    Hashtbl.add inttovar j v ;
    counter <- counter + 1

  (* var -> int *)
  method vartoint (v : Cudf.package) : int =
    try Cudf_hashtbl.find vartoint v
    with Not_found -> assert false
      
  (* int -> var *)
  method inttovar (i : int) : Cudf.package =
    try Hashtbl.find inttovar i 
    with Not_found -> assert false
end 

(** additional Cudf indexes
 *
 * the Cudf library does not consider features of packages that are not
 * installed. who_provides is a lookup function that returns __all__ packages
 * (installed or not) that implement a given feature 
 *
 * who_conflicts is a lookup function that returns all packages that conflict
 * with a given package 
 *
 * XXX: depsolver_int.ml implements similar indexes. This is a minor code
 * duplication.
 * *)

type maps = {
  (* the list of all packages the explicitely or implicitely
   * conflict with the given package *)
  who_conflicts : Cudf.package -> Cudf.package list;

  (* the list of all packages that provide the given feature *)
  who_provides : Cudf_types.vpkg -> Cudf.package list ;

  (* the list of all packages that satisfy the give package 
   * constraint. If there are no real packages, then the contraint
   * is interpreted as a feature request as in who_provides *)
  lookup_packages : Cudf_types.vpkg -> Cudf.package list ;

  (* assign an integer to each cudf package *)
  map : projection
}

let build_maps universe =
  let size = Cudf.universe_size universe in
  let conflicts = Cudf_hashtbl.create (2 * size) in
  let provides = Hashtbl.create (2 * size) in
  let map = new projection in

  Cudf.iter_packages (fun pkg ->
    map#add pkg;
    List.iter (function
      |name, None -> Hashtbl.add provides name (pkg, None)
      |name, Some (_, ver) -> Hashtbl.add provides name (pkg, (Some ver))
    ) pkg.provides
  ) universe
  ;

  let who_provides (pkgname,constr) =
    List.filter_map (function
      |pkg, None -> Some(pkg)
      |pkg, Some v when Cudf.version_matches v constr -> Some(pkg)
      |_,_ -> None
    ) (Hashtbl.find_all provides pkgname)
  in

  let lookup_packages (pkgname,constr) =
    match Cudf.lookup_packages ~filter:constr universe pkgname with
    |[] -> who_provides (pkgname,constr)
    |l -> l
  in

  (* we need to iterate twice on the package list as we need the
   * list of all virtual packages in order to generate the conflict list *)
  Cudf.iter_packages (fun pkg ->
    List.iter (fun (name,constr) ->
      List.iter (fun p ->
        if p <> pkg then begin
          Cudf_hashtbl.add conflicts pkg p ;
          Cudf_hashtbl.add conflicts p pkg
        end
      ) (lookup_packages (name,constr))
    ) pkg.conflicts
  ) universe
  ;

  let who_conflicts pkg = List.unique ~cmp:equal (Cudf_hashtbl.find_all conflicts pkg) in

  {
    who_conflicts = who_conflicts ;
    who_provides = who_provides ;
    lookup_packages = lookup_packages ;
    map = map
  }
;;
