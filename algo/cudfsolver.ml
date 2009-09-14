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

open ExtLib
open Depsolver_int
open Common
open CudfAdd

type solver = {
  mdf : Mdf.universe ;
  solver : Depsolver_int.solver
}

(*
     - A package that is installed can be replaced by the same package with a
       different version.
     - A package A can be replaced by a different package B that offers the same
       functionalities specified via provides
*)

let installed mdf =
  let maps = mdf.Mdf.maps in
  let index = mdf.Mdf.index in
  Array.fold_left (fun ll i ->
    let pkg = i.Mdf.pkg in
    if pkg.Cudf.installed then
      let pl1 = List.fold_left (fun l vpkg ->
        (maps.who_provides (vpkg :> Cudf_types.vpkg)) @ l
        ) [] pkg.Cudf.provides
      in
      let pl2 = maps.lookup_packages (pkg.Cudf.package,None) in
      let pl = List.map maps.to_sat (List.unique (pl1 @ pl2)) in
      if pl <> [] then pl :: ll else ll
    else ll
  ) [] index

let __init installed solver mdf = 
  let add l exp = S.add_rule solver.constraints (Array.of_list l) exp in
  let proxy_var = mdf.Mdf.maps.size in
  let proxy_lit = S.lit_of_var proxy_var false in
  List.iter (fun l ->
    let lit_pos_list = List.map (fun id -> S.lit_of_var id true) l in
    add (proxy_lit :: lit_pos_list) [Diagnostic_int.Installed_alternatives l]
  ) installed
  ;
  solver
;;

let __prepare request solver mdf =
  let add l exp = S.add_rule solver.constraints (Array.of_list l) exp in
  let proxy_var = mdf.Mdf.maps.size in
  let proxy_lit = S.lit_of_var proxy_var false in

  List.iter (fun vpkg ->
    let alternatives = List.map mdf.Mdf.maps.to_sat (mdf.Mdf.maps.lookup_packages vpkg) in
    (* let l = (mdf.Mdf.maps.lookup_packages vpkg) in
       Printf.printf "%s -> %s\n%!" (Cudf_types.string_of_vpkg vpkg) (String.concat ", " (List.map CudfAdd.print_package l)) ; *)
    let lit_pos_list = List.map (fun id -> S.lit_of_var id true) alternatives in
    add (proxy_lit :: lit_pos_list) [Diagnostic_int.To_install (vpkg, alternatives)];
  ) request.Cudf.install
  ;

  List.iter (fun vpkg ->
    let alternatives = List.map mdf.Mdf.maps.to_sat (mdf.Mdf.maps.lookup_packages vpkg) in
    let lit_pos_list = List.map (fun id -> S.lit_of_var id true) alternatives in
    add (proxy_lit :: lit_pos_list) [Diagnostic_int.To_upgrade (vpkg, alternatives)];
    if (List.length alternatives) > 1 then
      let lit_neg_list = List.map (fun id -> S.lit_of_var id false) alternatives in
      add (proxy_lit :: lit_neg_list) [Diagnostic_int.To_upgrade_singleton (vpkg,alternatives)];
  ) request.Cudf.upgrade
  ;

  List.iter (fun vpkg ->
    let alternatives = List.map mdf.Mdf.maps.to_sat (mdf.Mdf.maps.lookup_packages vpkg) in
    List.iter (fun id ->
      let lit = S.lit_of_var id false in
      S.add_bin_rule solver.constraints proxy_lit lit [Diagnostic_int.To_remove (vpkg,id)];
    ) alternatives
  ) request.Cudf.remove
  ;

  solver
;;

let load universe =
  match Cudf_checker.is_consistent universe with
  |true,None ->
      let mdf = Mdf.load_from_universe universe in
      let solver = Depsolver_int.init_solver mdf.Mdf.index in
      { mdf = mdf; solver = solver }
  |false,Some(r) -> begin
      Printf.eprintf "%s"
      (Cudf_checker.explain_reason (r :> Cudf_checker.bad_solution_reason)) ;
      exit(1)
  end
  |_,_ -> assert false

(* here we don't make any assumption to the freshness of the package that
   is going to be installed, upgraded or removed. The user should specify it
   as a constraint in the request *)
let init ?(buffer=false) universe request =
  let mdf = Mdf.load_from_universe universe in
  let alternatives vpkglist =
    List.map (fun vpkg -> 
      List.map mdf.Mdf.maps.to_sat (mdf.Mdf.maps.lookup_packages vpkg)
    ) vpkglist
  in

  let l_install = alternatives request.Cudf.install in
  let l_upgrade = alternatives request.Cudf.upgrade in
  let l_remove = alternatives request.Cudf.remove in
  let l_request = (l_install @ l_upgrade @ l_remove ) in
  let l_installed = installed mdf in
  let idlist = List.flatten (l_request @ l_installed) in
  let closure = Depsolver_int.dependency_closure mdf.Mdf.index idlist in
  let solver = Depsolver_int.init_solver
      ~buffer:buffer
      ~proxy_size:1
      ~idlist:closure
      mdf.Mdf.index 
  in
  let solver = __init l_installed solver mdf in
  let solver = __prepare request solver mdf in
  { mdf = mdf; solver = solver }

let reason maps =
  List.map (function
    |Diagnostic_int.Dependency(i,il) ->
        Diagnostic.Dependency(maps.from_sat i,List.map maps.from_sat il)
    |Diagnostic_int.EmptyDependency(i,vl) ->
        Diagnostic.EmptyDependency(maps.from_sat i,vl)
    |Diagnostic_int.Conflict(i,j) ->
        Diagnostic.Conflict(maps.from_sat i,maps.from_sat j)
    |Diagnostic_int.Installed_alternatives(il) ->
        Diagnostic.Installed_alternatives(List.map maps.from_sat il)
    |Diagnostic_int.To_install(v,il) ->
        Diagnostic.To_install(v,List.map maps.from_sat il)
    |Diagnostic_int.To_remove(v,i) ->
        Diagnostic.To_remove(v,maps.from_sat i)
    |Diagnostic_int.To_upgrade(v,il) ->
        Diagnostic.To_upgrade(v,List.map maps.from_sat il)
    |Diagnostic_int.To_upgrade_singleton(v,il) ->
        Diagnostic.To_upgrade_singleton(v,List.map maps.from_sat il)
  )

let result maps = function
  |Diagnostic_int.Success f ->
      Diagnostic.Success (fun () ->
        List.map (fun i ->
          {(maps.from_sat i) with Cudf.installed = true}
        ) (f ())
      )
  |Diagnostic_int.Failure f -> Diagnostic.Failure (fun () -> reason maps (f ()))

let request maps = function
  |Diagnostic_int.Sng i -> Diagnostic.Package (maps.from_sat i)
  |Diagnostic_int.Lst il -> Diagnostic.PackageList (List.map maps.from_sat il)
  |Diagnostic_int.Req i -> Diagnostic.Proxy

let diagnosis maps res req =
  let result = result maps res in
  let request = request maps req in
  { Diagnostic.result = result ; request = request }

let solve s =
  let maps = s.mdf.Mdf.maps in
  let proxy_var = s.mdf.Mdf.maps.size in
  let req = Diagnostic_int.Req(proxy_var) in
  let res = Depsolver_int.solve s.solver req in
  diagnosis maps res req

let dump s =
  Depsolver_int.S.dump s.solver.constraints
(*
   let s = Depsolver_int.S.dump s.solver.constraints in
  let input = IO.input_string s in
  while true do
    let line = IO.read_line input in
    Str.split (
      *)
