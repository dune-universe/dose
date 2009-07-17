(*****************************************************************************)
(*  Copyright (C) 2009  <pietro.abate@pps.jussieu.fr>                        *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

open IprLib
open ExtLib

open Cudf
open Depsolver_int

type solver = Depsolver_int.solver

(*
     - A package that is installed can be replaced by the same package with a
       different version.
     - A package A can be replaced by a different package B that offers the same
       functionalities specified via provides
*)

let installed (cudf_universe,maps) =
  Cudf.fold_packages (fun ll pkg ->
    if pkg.installed then
      let pl1 = List.fold_left (fun l vpkg ->
        l @ (maps.who_provides (vpkg :> Cudf_types.vpkg))
        ) [] pkg.provides
      in
      let pl2 = maps.lookup_packages (pkg.package,None) in
      let pl = List.unique (pl1 @ pl2) in
      if pl <> [] then pl :: ll else ll
    else ll
  ) [] cudf_universe

let __init (cudf_universe,solver,maps)  = 
  let add l exp = S.add_rule solver.constraints (Array.of_list l) exp in
  let proxy_var = solver.size - 1 in
  let proxy_lit = S.lit_of_var proxy_var false in
  let installed = installed (cudf_universe,maps) in

  List.iter (fun l ->
    let lit_pos_list = List.map (fun pkg -> S.lit_of_var (maps.to_sat pkg) true) l in
    add (proxy_lit :: lit_pos_list) [Diagnostic.Installed_alternatives l]
  ) installed
  ;

  { solver = solver; maps = maps; universe = cudf_universe }
;;

let __prepare (cudf_universe,solver,maps) request =
  let add l exp = S.add_rule solver.constraints (Array.of_list l) exp in
  let proxy_var = solver.size - 1 in

  let proxy_lit = S.lit_of_var proxy_var false in

  List.iter (fun vpkg ->
    let alternatives = maps.lookup_packages vpkg in
    let lit_pos_list =
      List.map (fun pkg -> S.lit_of_var (maps.to_sat pkg) true) alternatives
    in
    add (proxy_lit :: lit_pos_list) [Diagnostic.To_install (vpkg, alternatives)];
  ) request.install
  ;

  List.iter (fun vpkg ->
    let alternatives = maps.lookup_packages vpkg in
    let lit_pos_list =
      List.map (fun pkg -> S.lit_of_var (maps.to_sat pkg) true) alternatives
    in
    let lit_neg_list =
      List.map (fun pkg -> S.lit_of_var (maps.to_sat pkg) false) alternatives
    in
    add (proxy_lit :: lit_pos_list) [Diagnostic.To_upgrade (vpkg, alternatives)];
    if (List.length alternatives) > 1 then begin
      add (proxy_lit :: lit_neg_list) [Diagnostic.To_upgrade_singleton (vpkg,alternatives)];
    end ;
  ) request.upgrade
  ;

  List.iter (fun vpkg ->
    List.iter (fun pkg ->
      let lit = S.lit_of_var (maps.to_sat pkg) false in
      S.add_bin_rule solver.constraints proxy_lit lit [Diagnostic.To_remove (vpkg,pkg)];
    ) (maps.lookup_packages vpkg)
  ) request.remove
  ;

  { solver = solver; maps = maps; universe = cudf_universe }
;;

let reinit s request =
  let solver = Depsolver_int.init_solver false 1 (s.universe,s.maps) in
  let s1 = __init (s.universe,solver,s.maps) in
  let cudf_universe = s1.universe in
  let solver = s1.solver in
  let maps = s1.maps in
  __prepare (cudf_universe,solver,maps) request

(* return a new universe where I don't need to recompute the maps
 * as I know that it is a subset of the old universe *)
let reduce s request =
  let alternatives l =
    List.flatten (List.map (fun vpkg -> s.maps.lookup_packages vpkg) l )
  in
  let l = 
    ( alternatives request.install ) @
    ( alternatives request.upgrade ) @
    ( alternatives request.remove )
  in
  let installed = List.flatten (installed (s.universe,s.maps)) in
  let u = Depsolver_int.dependency_closure s.maps (installed @ l) in
  reinit {s with universe = Cudf.load_universe u} request

(* here we don't make any assumption to the freshness of the package that
   is going to be installed, upgraded or removed. The use should specify it
   as a constraint in the request *)
let init ?(buffer=false) cudf_universe request =
  let maps = Depsolver_int.build_maps cudf_universe in
  let alternatives l =
    List.flatten (List.map (fun vpkg -> maps.lookup_packages vpkg) l )
  in
  let l = 
    ( alternatives request.install ) @
    ( alternatives request.upgrade ) @
    ( alternatives request.remove )
  in
  let installed = List.flatten (installed (cudf_universe,maps)) in
  let u = Depsolver_int.dependency_closure maps (installed @ l) in
  let cudf_universe = Cudf.load_universe u in
  let maps = Depsolver_int.build_maps cudf_universe in
  let solver = Depsolver_int.init_solver buffer 1 (cudf_universe,maps) in
  let s = __init (cudf_universe,solver,maps) in
  let cudf_universe = s.universe in
  let solver = s.solver in
  let maps = s.maps in
  __prepare (cudf_universe,solver,maps) request

let solve s =
  let solver = s.solver in
  let maps = s.maps in
  Depsolver_int.solve (solver,maps) (Diagnostic.Req)

let dump s =
  Depsolver_int.S.dump s.solver.constraints
