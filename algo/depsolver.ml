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
open Common
open CudfAdd

let debug fmt = Util.make_debug "Depsolver" fmt
let info fmt = Util.make_info "Depsolver" fmt
let warning fmt = Util.make_warning "Depsolver" fmt
let fatal fmt = Util.make_fatal "Depsolver" fmt

type solver = {
  maps : Cudf.universe ;
  solver : Depsolver_int.solver
}

(** 
 * @param check if the universe is consistent *)
let load ?(check=true) universe =
  let is_consistent check universe =
    if check then Cudf_checker.is_consistent universe
    else (true,None)
  in
  match is_consistent check universe with
  |true,None ->
      let solver = Depsolver_int.init_solver universe in
      { maps = universe ; solver = solver }
  |false,Some(r) -> 
      fatal "%s"
      (Cudf_checker.explain_reason (r :> Cudf_checker.bad_solution_reason)) ;
  |_,_ -> assert false

let reason universe =
  let from_sat = CudfAdd.inttovar universe in
  List.map (function
    |Diagnostic_int.Dependency(i,vl,il) ->
        Diagnostic.Dependency(from_sat i,vl,List.map from_sat il)
    |Diagnostic_int.Missing(i,vl) ->
        Diagnostic.Missing(from_sat i,vl)
    |Diagnostic_int.Conflict(i,j) ->
        Diagnostic.Conflict(from_sat i,from_sat j)
  )

let result universe result = 
  let from_sat = CudfAdd.inttovar universe in
  match result with
  |Diagnostic_int.Success f_int ->
      Diagnostic.Success (fun ?(all=false) () ->
        List.map (fun i ->
          {(from_sat i) with Cudf.installed = true}
        ) (f_int ~all ())
      )
  |Diagnostic_int.Failure f -> Diagnostic.Failure (fun () ->
      reason universe (f ()))

let request universe result = 
  let from_sat = CudfAdd.inttovar universe in
  match result with
  |Diagnostic_int.Sng i -> Diagnostic.Package (from_sat i)
  |Diagnostic_int.Lst il -> Diagnostic.PackageList (List.map from_sat il)

let diagnosis universe res req =
  let result = result universe res in
  let request = request universe req in
  { Diagnostic.result = result ; request = request }

let univcheck ?callback universe =
  match callback with
  |None -> Depsolver_int.univcheck universe
  |Some f ->
      let callback_int (res,req) = f (diagnosis universe res req) in
      Depsolver_int.univcheck ~callback:callback_int universe

let listcheck ?callback universe pkglist =
  let idlist = List.map (CudfAdd.vartoint universe) pkglist in
  match callback with
  |None -> Depsolver_int.listcheck universe idlist
  |Some f ->
      let callback_int (res,req) = f (diagnosis universe res req) in
      Depsolver_int.listcheck ~callback:callback_int universe idlist

let edos_install s pkg =
  let req = Diagnostic_int.Sng (CudfAdd.vartoint s.maps pkg) in
  let res = Depsolver_int.solve s.solver req in
  diagnosis s.maps res req

let edos_coinstall s pkglist =
  let idlist = List.map (CudfAdd.vartoint s.maps) pkglist in
  let req = Diagnostic_int.Lst idlist in
  let res = Depsolver_int.solve s.solver req in
  diagnosis s.maps res req

let trim universe =
  let trimmed_pkgs = ref [] in
  let callback d =
    if Diagnostic.is_solution d then
      match d.Diagnostic.request with
      |Diagnostic.Package p -> trimmed_pkgs := p::!trimmed_pkgs
      |_ -> assert false
  in
  ignore (univcheck ~callback universe);
  Cudf.load_universe !trimmed_pkgs

let dependency_closure ?maxdepth ?conjunctive univ pkglist =
  let idlist = List.map (CudfAdd.vartoint univ) pkglist in
  let closure = Depsolver_int.dependency_closure ?maxdepth ?conjunctive univ idlist in
  List.map (CudfAdd.inttovar univ) closure

let reverse_dependencies univ =
  let rev = Depsolver_int.reverse_dependencies univ in
  let h = Cudf_hashtbl.create (Array.length rev) in
  Array.iteri (fun i l ->
    Cudf_hashtbl.add h 
      (CudfAdd.inttovar univ i) 
      (List.map (CudfAdd.inttovar univ) l)
  ) rev ;
  h

let reverse_dependency_closure ?maxdepth univ pkglist =
  let idlist = List.map (CudfAdd.vartoint univ) pkglist in
  let reverse = Depsolver_int.reverse_dependencies univ in
  let closure = Depsolver_int.reverse_dependency_closure ?maxdepth reverse idlist in
  List.map (CudfAdd.inttovar univ) closure

type enc = Cnf | Dimacs

let output_clauses ?(enc=Cnf) univ =
  let solver = Depsolver_int.init_solver ~buffer:true univ in
  let clauses = Depsolver_int.S.dump solver.Depsolver_int.constraints in
  let buff = Buffer.create (Cudf.universe_size univ) in
  let to_cnf dump =
    let str (v, p) = 
      let pkg = (CudfAdd.inttovar univ) (abs v) in
      let pol = if p then "" else "!" in
      Printf.sprintf "%s%s-%d" pol pkg.Cudf.package pkg.Cudf.version
    in
    List.iter (fun l ->
      List.iter (fun var -> Printf.bprintf buff " %s" (str var)) l;
      Printf.bprintf buff "\n"
    ) dump
  in
  let to_dimacs dump =
    let str (v, p) =
      if p then Printf.sprintf "%d" v else Printf.sprintf "-%d" v in
    let varnum = Cudf.universe_size univ in
    let closenum = (List.length clauses) in
    Printf.bprintf buff "p cnf %d %d\n" varnum closenum;
    List.iter (fun l ->
      List.iter (fun var -> Printf.bprintf buff " %s" (str var)) l;
      Printf.bprintf buff " 0\n"
    ) dump
  in
  if enc = Cnf then to_cnf clauses ;
  if enc = Dimacs then to_dimacs clauses;
  Buffer.contents buff
;;

(** check if a cudf request is satisfiable. we do not care about
 * universe consistency . We try to installa dummy package *)
let check_request (_,pkglist,request) =
  let deps = 
    let k = 
      List.filter_map (fun pkg ->
        if pkg.Cudf.installed then
          match pkg.Cudf.keep with
          |`Keep_package -> Some(pkg.Cudf.package,None)
          |`Keep_version -> Some(pkg.Cudf.package,Some(`Eq,pkg.Cudf.version))
          |_ -> None
        else None
      ) pkglist
    in
    let l = request.Cudf.install @ request.Cudf.upgrade in
    debug "request consistency (keep %d) (install %d) (upgrade %d) (remove %d) (# %d)"
    (List.length k) (List.length request.Cudf.install) 
    (List.length request.Cudf.upgrade)
    (List.length request.Cudf.remove)
    (List.length pkglist);
    List.map (fun j -> [j]) (l @ k) 
  in
  let dummy = {
    Cudf.default_package with
    Cudf.package = "dummy";
    version = 1;
    depends = deps;
    conflicts = request.Cudf.remove}
  in
  let universe = Cudf.load_universe (dummy::pkglist) in
  let solver = load ~check:false universe in
  edos_install solver dummy

