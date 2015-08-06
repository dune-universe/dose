(**************************************************************************************)
(*  Copyright (C) 2015 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2015 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Opam/Pef format conversion routines *)

open ExtLib
open Common

(* ========================================= *)

type extramap = (string * (string * Cudf_types.typedecl1)) list

let preamble = 
  (* number is a mandatory property -- no default *)
  let l = [
    ("recommends",(`Vpkgformula (Some [])));
    ("number",(`String None)) ]
  in
  CudfAdd.add_properties Cudf.default_preamble l

let add_extra extras tables pkg =
  let number = ("number",`String pkg#version) in
  let l =
    List.filter_map (fun (debprop, (cudfprop,v)) ->
      let debprop = String.lowercase debprop in
      let cudfprop = String.lowercase cudfprop in
      try 
        let s = List.assoc debprop pkg#extras in
        let typ = Cudf_types.type_of_typedecl v in
        Some (cudfprop, Cudf_types_pp.parse_value typ s)
      with Not_found -> None
    ) extras
  in
  let recommends = ("recommends", `Vpkgformula (Pef.Pefcudf.loadll tables pkg#recommends)) in

  List.filter_map (function
    |(_,`Vpkglist []) -> None
    |(_,`Vpkgformula []) -> None
    |e -> Some e
  )
  [number; recommends] @ l
;;

let tocudf tables ?(extras=[]) ?(extrasfun=(fun _ _ -> [])) ?(inst=false) pkg =
    { Cudf.default_package with
      Cudf.package = CudfAdd.encode pkg#name ;
      Cudf.version = Pef.Pefcudf.get_cudf_version tables (pkg#name,pkg#version) ;
      Cudf.depends = Pef.Pefcudf.loadll tables pkg#depends;
      Cudf.conflicts = Pef.Pefcudf.loadlc tables pkg#name pkg#conflicts;
      Cudf.provides = Pef.Pefcudf.loadlp tables pkg#provides ;
      Cudf.pkg_extra = (add_extra extras tables pkg)@(extrasfun tables pkg) ;
    }

let load_list compare l =
  let timer = Util.Timer.create "Opam.ToCudf" in
  Util.Timer.start timer;
  let tables = Pef.Pefcudf.init_tables compare l in
  let pkglist = List.map (tocudf tables) l in
  Pef.Pefcudf.clear tables;
  Util.Timer.stop timer pkglist

let load_universe compare l =
  let pkglist = load_list compare l in
  let timer = Util.Timer.create "Opam.ToCudf" in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
