(**************************************************************************************)
(*  Copyright (C) 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2010 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)


open ExtLib

module Cudf_set = CudfAdd.Cudf_set
module StringSet = CudfAdd.StringSet

type solution = {
  installed : Cudf_set.t ;
  removed : Cudf_set.t ;
  unchanged : Cudf_set.t
}

(* the 'package' is always taken from the universe *)
let to_set univ l =
  List.fold_left (fun s p ->
    let q = Cudf.lookup_package univ (p.Cudf.package,p.Cudf.version) in
    Cudf_set.add q s
  ) Cudf_set.empty l
;;

(* the list of all packages (versions) that were installed before
 * but not now *)
let removed univ sol pkgname =
  let were_installed = to_set univ (Cudf.get_installed univ pkgname) in
  let are_installed = to_set univ (Cudf.get_installed sol pkgname) in
  Cudf_set.diff were_installed are_installed

(* the list of all packages (versions) that were not installed before
 * but are installed now *)
let installed univ sol pkgname =
  let were_installed = to_set univ (Cudf.get_installed univ pkgname) in
  let are_installed = to_set univ (Cudf.get_installed sol pkgname) in
  Cudf_set.diff are_installed were_installed

(* for each pkgname I've the list of all versions that were installed, removed
 * or left unchanged *)
let diff univ sol =
  let pkgnames = CudfAdd.pkgnames univ in
  let h = Hashtbl.create (StringSet.cardinal pkgnames) in
  StringSet.iter (fun pkgname ->
    let a = CudfAdd.to_set (Cudf.lookup_packages univ pkgname) in
    let r = removed univ sol pkgname in
    let i = installed univ sol pkgname in
    let u = Cudf_set.diff a (Cudf_set.union r i) in
    let s = { removed = r ; installed = i ; unchanged = u } in
    Hashtbl.add h pkgname s
  ) pkgnames ;
  h

type t =
  |Un of Cudf_set.t (* unchanged *)
  |Rm of Cudf_set.t (* removed *)
  |In of Cudf_set.t (* installed *)
  |Up of Cudf_set.t (* upgraded *)
  |Dw of Cudf_set.t (* downgraded *)

let uniqueversion all s =
  let l = ref [] in
  let i = Cudf_set.filter (fun pkg -> pkg.Cudf.installed) all in
  if (Cudf_set.cardinal i <= 1) && ((Cudf_set.cardinal s.installed) <= 1) then
    begin
      if (Cudf_set.cardinal s.installed) = 1 then begin
        if (Cudf_set.cardinal i) = 1 then begin
          let np = Cudf_set.choose i in
          let op = Cudf_set.choose s.installed in
          if np.Cudf.version < op.Cudf.version
          then l := Up(s.installed)::!l
          else l := Dw(s.installed)::!l
        end
        else
          l := In(s.installed)::!l
      end;
      if not (Cudf_set.is_empty s.unchanged) then
        l := Un(s.unchanged)::!l;
      if not (Cudf_set.is_empty s.removed) then
        l := Rm(s.removed)::!l ;
    end
  else begin
    if not (Cudf_set.is_empty s.unchanged) then
      l := Un(s.unchanged)::!l;
    if not (Cudf_set.is_empty s.removed) then
      l := Rm(s.removed)::!l ;
    if not (Cudf_set.is_empty s.installed) then
      l := In(s.installed)::!l ;
  end;
  !l
;;

