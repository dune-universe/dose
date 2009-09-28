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
open Common
open CudfAdd

type solver = {
  mdf : Mdf.universe ;
  solver : Depsolver_int.solver
}

let load universe =
  match Cudf_checker.is_consistent universe with
  |true,None ->
      let mdf = Mdf.load_from_universe universe in
      let solver = Depsolver_int.init_solver mdf.Mdf.index in
      { mdf = mdf ; solver = solver }
  |false,Some(r) -> begin
      Printf.eprintf "%s"
      (Cudf_checker.explain_reason (r :> Cudf_checker.bad_solution_reason)) ;
      exit(1)
  end
  |_,_ -> assert false

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
  |Diagnostic_int.Req i -> Diagnostic.Proxy

let diagnosis maps res req =
  let result = result maps res in
  let request = request maps req in
  { Diagnostic.result = result ; request = request }

let univcheck ?callback s =
  let maps = s.mdf.Mdf.maps in
  match callback with
  |None -> Depsolver_int.univcheck (s.mdf,s.solver)
  |Some f ->
      let callback_int (res,req) = f (diagnosis maps res req) in
      Depsolver_int.univcheck ~callback:callback_int (s.mdf,s.solver)

let edos_install s pkg =
  let maps = s.mdf.Mdf.maps in
  let req = Diagnostic_int.Sng (maps.map#vartoint pkg) in
  let res = Depsolver_int.solve s.solver req in
  diagnosis maps res req

let edos_coinstall s pkglist =
  let maps = s.mdf.Mdf.maps in
  let idlist = List.map maps.map#vartoint pkglist in
  let req = Diagnostic_int.Lst idlist in
  let res = Depsolver_int.solve s.solver req in
  diagnosis maps res req

let dependency_closure universe pkglist =
  let mdf = Mdf.load_from_universe universe in
  let maps = mdf.Mdf.maps in
  let idlist = List.map maps.map#vartoint pkglist in
  let closure = Depsolver_int.dependency_closure mdf idlist in
  List.map maps.map#inttovar closure

let reverse_dependencies universe =
  let mdf = Mdf.load_from_universe universe in
  let maps = mdf.Mdf.maps in
  let rev = Depsolver_int.reverse_dependencies mdf in
  let h = Cudf_hashtbl.create (Array.length rev) in
  Array.iteri (fun i l ->
    Cudf_hashtbl.add h (maps.map#inttovar i) (List.map maps.map#inttovar l)
  ) rev ;
  h

let reverse_dependency_closure universe pkglist =
  let mdf = Mdf.load_from_universe universe in
  let maps = mdf.Mdf.maps in
  let idlist = List.map maps.map#vartoint pkglist in
  let reverse = Depsolver_int.reverse_dependencies mdf in
  let closure = Depsolver_int.reverse_dependency_closure reverse idlist in
  List.map maps.map#inttovar closure

