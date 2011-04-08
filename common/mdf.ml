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

(** Internal data package data format. Packages dependencies and conflicts are
    all expanded and explicit *)

open ExtLib
open CudfAdd

let progressbar = Util.Progress.create "Mdf.__load"

type package = {
  id : int ; (** package id relative to the universe *)
  pkg : Cudf.package; (** cudf package *)
  depends : (Cudf_types.vpkg list * int list * Cudf.package list) list ;
  conflicts : (Cudf.package * int) list (** cudf package and package id *)
}

type universe = {
  index : package array ; (** the array index is equal to the package id *)
  maps : CudfAdd.maps (** maps to associate a cudf package to an id *)
}

let default_package = {
  id = 0;
  pkg = Cudf.default_package;
  depends = [] ;
  conflicts = [] 
}

let __load maps universe =
  let cmp : int -> int -> bool = (=) in
  let to_sat = maps.map#vartoint in
  let size = Cudf.universe_size universe in
  let a = Array.create size default_package in
  Util.Progress.set_total progressbar size ;
  Cudf.iter_packages (fun pkg ->
    Util.Progress.progress progressbar;
    let id = to_sat pkg in
    let cl =
      List.map (fun p ->
        (p,to_sat p)
      ) (maps.who_conflicts pkg)
    in
    let dll =
      List.map (fun disjunction ->
        let (l1,l2,l3) =
          List.fold_left (fun (l1,l2,l3) vpkg ->
            let l = maps.who_provides vpkg in
            let el = List.fold_left (fun acc i -> (to_sat i)::acc) l2 l in
            let dl = List.fold_left (fun acc i -> i::acc) l3 l in
            (vpkg::l1,el,dl)
          ) ([],[],[]) disjunction
        in (l1,List.unique ~cmp l2, List.unique ~cmp:Cudf.(=%) l3)
      ) pkg.Cudf.depends
    in
    let p = {
      id = id;
      pkg = pkg;
      depends = dll;
      conflicts = cl
    }
    in
    a.(id) <- p
  ) universe;
  Util.Progress.reset progressbar;
  a
;;

(** trasfrom a cudf package list in a Mdf universe. All references are
    explicit and given in terms of integer *)
let load_from_list pkglist =
  let universe = Cudf.load_universe pkglist in
  let maps = build_maps universe in
  let index = __load maps universe in
  { index = index ; maps = maps }

(** transform a cudf universe in a mdf universe. All references are
    explicit and given in terms of integer *)
let load_from_universe universe =
  let maps = build_maps universe in
  let index = __load maps universe in
  { index = index ; maps = maps }
