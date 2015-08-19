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

type options = {
  switch : string ; (* the active switch *)
  switches : string list ; (* list of available switches *)
  profiles : string list (* list of build profiles *)
}

let default_options = {
  switch = "system";
  switches = [];
  profiles = [];
}

let preamble = 
  (* number is a mandatory property -- no default *)
  let l = [
    ("recommends",(`Vpkgformula (Some [])));
    ("number",(`String None));
    ]
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

(* each package generates more than one cudf package. One for active switch
   that is not declaclare not available by the package . Each package is 
   translated differently considering the profiles associated to each dependency *)
let tocudf tables ?(options=default_options) ?(extras=[]) pkg =
  List.fold_left (fun acc switch ->
    (* include this package if it is not declared as not available and if it is
     * used in some dependency. Otherwise there is no point to include it *)
    if List.mem "all" pkg#switch || List.mem switch pkg#switch then
      let cudfpkg = 
        { Cudf.default_package with
          Cudf.package = CudfAdd.encode (switch^":"^pkg#name);
          Cudf.version = Pef.Pefcudf.get_cudf_version tables (pkg#name,pkg#version) ;
          Cudf.depends = Pef.Pefcudf.loadll tables pkg#depends;
          Cudf.conflicts = Pef.Pefcudf.loadlc tables pkg#name pkg#conflicts;
          Cudf.provides = Pef.Pefcudf.loadlp tables pkg#provides ;
          Cudf.pkg_extra = add_extra extras tables pkg ;
        }
      in (cudfpkg::acc)
    else acc
  ) [] (options.switch::options.switches)

let load_list compare l =
  let timer = Util.Timer.create "Opam.ToCudf" in
  Util.Timer.start timer;
  let tables = Pef.Pefcudf.init_tables compare l in
  let pkglist = List.flatten (List.map (tocudf tables) l) in
  Pef.Pefcudf.clear tables;
  Util.Timer.stop timer pkglist

let load_universe compare l =
  let pkglist = load_list compare l in
  let timer = Util.Timer.create "Opam.ToCudf" in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
