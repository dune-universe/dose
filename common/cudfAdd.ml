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

(* I want to hash packages by name/version without considering
   other fields like Installed / keep / etc.
*)
module Cudf_hashtbl =
  Hashtbl.Make(struct
    type t = Cudf.package
    let equal = Cudf.(=%)
    let hash pkg = Hashtbl.hash (pkg.Cudf.package, pkg.Cudf.version)
  end)

open ExtLib

let parse_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    Cudf_parser.load p
  with
    Cudf_parser.Parse_error _
    | Cudf.Constraint_violation _ as exn ->
      Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
      doc (Printexc.to_string exn);
      exit 1

(** additional Cudf indexes *)
type maps = {
  who_conflicts : Cudf.package -> Cudf.package list;
  who_provides : Cudf_types.vpkg -> Cudf.package list
}

let build_maps universe =
  let size = Cudf.universe_size universe in
  let conflicts = Hashtbl.create (2 * size) in
  let provides = Hashtbl.create (2 * size) in
  Cudf.iter_packages (fun pkg ->
    List.iter (function
      |name, None -> Hashtbl.add provides name (pkg, None)
      |name, Some (_, ver) -> Hashtbl.add provides name (pkg, (Some ver))
    ) pkg.provides
  ) universe

  ;

  let who_provides (pkgname,constr) =
    match Cudf.lookup_packages ~filter:constr universe pkgname with
    |[] ->
        List.filter_map (function
          |pkg, None -> Some(pkg)
          |pkg, Some v when Cudf.version_matches v constr -> Some(pkg)
          |_,_ -> None
        ) (Hashtbl.find_all provides pkgname)
    |l -> l
  in

  Cudf.iter_packages (fun pkg ->
    List.iter (fun (name,constr) ->
      List.iter (fun p ->
        Hashtbl.add conflicts pkg p ;
        Hashtbl.add conflicts p pkg
      ) (who_provides (name,constr))
    ) pkg.conflicts
  ) universe
  ;

  let who_conflicts pkg = List.unique (Hashtbl.find_all conflicts pkg) in

  {
    who_conflicts = who_conflicts ;
    who_provides = who_provides ;
  }

