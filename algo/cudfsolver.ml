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
  let vartoint = maps.map#vartoint in
  let index = mdf.Mdf.index in
  Array.fold_left (fun ll i ->
    let pkg = i.Mdf.pkg in
    if pkg.Cudf.installed then
      let pl1 = List.fold_left (fun l vpkg ->
        (maps.lookup_virtual (vpkg :> Cudf_types.vpkg)) @ l
        ) [] pkg.Cudf.provides
      in
      let pl2 = maps.who_provides (pkg.Cudf.package,None) in
      let pl = List.map vartoint (List.unique (pl1 @ pl2)) in
      if pl <> [] then pl :: ll else ll
    else ll
  ) [] index

let __init installed solver mdf = 
  let add l exp = S.add_rule solver.constraints (Array.of_list l) exp in
  let proxy_var = solver.Depsolver_int.proxy 0 in
  let proxy_lit = S.lit_of_var proxy_var false in
  List.iter (fun l ->
    let lit_pos_list = List.map (fun id -> S.lit_of_var (solver.Depsolver_int.map#vartoint id) true) l in
    add (proxy_lit :: lit_pos_list) [Diagnostic_int.Installed_alternatives l]
  ) installed
  ;
  solver
;;

let __prepare request solver mdf =
  let maps = mdf.Mdf.maps in
  let to_sat = maps.map#vartoint in

  let add l exp = S.add_rule solver.constraints (Array.of_list l) exp in
  let proxy_var = solver.Depsolver_int.proxy 0 in
  let proxy_lit = S.lit_of_var proxy_var false in
  let lit_list polarity alternatives = 
    List.map (fun id ->
      S.lit_of_var (solver.Depsolver_int.map#vartoint id) polarity
    ) alternatives
  in

  List.iter (fun vpkg ->
    let alternatives = List.map to_sat (mdf.Mdf.maps.who_provides vpkg) in
    let lit_pos_list = lit_list true alternatives in
    add (proxy_lit :: lit_pos_list) [Diagnostic_int.To_install (vpkg, alternatives)];
  ) request.Cudf.install
  ;

  List.iter (fun vpkg ->
    let alternatives = List.map to_sat (mdf.Mdf.maps.who_provides vpkg) in
    let lit_pos_list = lit_list true alternatives in
    add (proxy_lit :: lit_pos_list) [Diagnostic_int.To_upgrade (vpkg, alternatives)];
    if (List.length alternatives) > 1 then
      let lit_neg_list = lit_list false alternatives in
      add (proxy_lit :: lit_neg_list) [Diagnostic_int.To_upgrade_singleton (vpkg,alternatives)];
  ) request.Cudf.upgrade
  ;

  List.iter (fun vpkg ->
    let alternatives = List.map to_sat (mdf.Mdf.maps.who_provides vpkg) in
    List.iter (fun id ->
      let lit = S.lit_of_var (solver.Depsolver_int.map#vartoint id) false in
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
      exit (-1)
  end
  |_,_ -> assert false

(* here we don't make any assumption to the freshness of the package that
   is going to be installed, upgraded or removed. The user should specify it
   as a constraint in the request *)
let load universe request =
  let mdf = Mdf.load_from_universe universe in
  let maps = mdf.Mdf.maps in
  let vartoint = maps.map#vartoint in

  let alternatives vpkglist =
    List.map (fun vpkg -> 
      List.map vartoint (mdf.Mdf.maps.who_provides vpkg)
    ) vpkglist
  in

  let l_install = alternatives request.Cudf.install in
  let l_upgrade = alternatives request.Cudf.upgrade in
  let l_remove = alternatives request.Cudf.remove in
  let l_request = (l_install @ l_upgrade @ l_remove ) in
  let l_installed = installed mdf in
  let idlist = List.flatten (l_request @ l_installed) in
  let closure = Depsolver_int.dependency_closure mdf idlist in
  let solver = Depsolver_int.init_solver
      ~proxy_size:1
      ~closure
      mdf.Mdf.index 
  in
  let solver = __init l_installed solver mdf in
  let solver = __prepare request solver mdf in
  { mdf = mdf; solver = solver }

let reason maps =
  let from_sat = maps.map#inttovar in
  List.map (function
    |Diagnostic_int.Dependency(i,il) ->
        Diagnostic.Dependency(from_sat i,List.map from_sat il)
    |Diagnostic_int.EmptyDependency(i,vl) ->
        Diagnostic.EmptyDependency(from_sat i,vl)
    |Diagnostic_int.Conflict(i,j) ->
        Diagnostic.Conflict(from_sat i,from_sat j)
    |Diagnostic_int.Installed_alternatives(il) ->
        Diagnostic.Installed_alternatives(List.map from_sat il)
    |Diagnostic_int.To_install(v,il) ->
        Diagnostic.To_install(v,List.map from_sat il)
    |Diagnostic_int.To_remove(v,i) ->
        Diagnostic.To_remove(v,from_sat i)
    |Diagnostic_int.To_upgrade(v,il) ->
        Diagnostic.To_upgrade(v,List.map from_sat il)
    |Diagnostic_int.To_upgrade_singleton(v,il) ->
        Diagnostic.To_upgrade_singleton(v,List.map from_sat il)
  )

let result maps result =
  let from_sat = maps.map#inttovar in
  match result with
  |Diagnostic_int.Success f ->
      Diagnostic.Success (fun () ->
        List.map (fun i ->
          {(from_sat i) with Cudf.installed = true}
        ) (f ())
      )
  |Diagnostic_int.Failure f -> Diagnostic.Failure (fun () -> reason maps (f ()))

let request maps result =
  let from_sat = maps.map#inttovar in
  match result with
  |Diagnostic_int.Sng i -> Diagnostic.Package (from_sat i)
  |Diagnostic_int.Lst il -> Diagnostic.PackageList (List.map from_sat il)
  |Diagnostic_int.Req _ -> Diagnostic.Proxy

let diagnosis maps res req =
  let result = result maps res in
  let request = request maps req in
  { Diagnostic.result = result ; request = request }

let solve s =
  let maps = s.mdf.Mdf.maps in
  let req = Diagnostic_int.Req 0 in
  let res = Depsolver_int.solve s.solver req in
  diagnosis maps res req

let dump s =
  Depsolver_int.S.dump s.solver.constraints
