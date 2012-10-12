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

include Util.Logging(struct let label = __FILE__ end) ;;

type solver = Depsolver_int.solver

(** 
 * @param check if the universe is consistent *)
let load ?(check=true) universe =
  let is_consistent check universe =
    if check then Cudf_checker.is_consistent universe
    else (true,None)
  in
  match is_consistent check universe with
  |true,None -> Depsolver_int.init_solver_univ universe 
  |false,Some(r) -> 
      fatal "%s"
      (Cudf_checker.explain_reason (r :> Cudf_checker.bad_solution_reason)) ;
  |_,_ -> assert false

let reason map universe =
  let from_sat = CudfAdd.inttovar universe in
  let globalid = Cudf.universe_size universe in
  List.filter_map (function
    |Diagnostic_int.Dependency(i,vl,il) when i = globalid -> None
    |Diagnostic_int.Missing(i,vl) when i = globalid -> 
        fatal "the package encoding global constraints can't be missing"
    |Diagnostic_int.Conflict(i,j,vpkg) when i = globalid || j = globalid -> 
        fatal "the package encoding global constraints can't be in conflict"

    |Diagnostic_int.Dependency(i,vl,il) -> Some (
        Diagnostic.Dependency(from_sat (map#inttovar i),vl,List.map (fun i -> from_sat (map#inttovar i)) il)
    )
    |Diagnostic_int.Missing(i,vl) -> Some (
        Diagnostic.Missing(from_sat (map#inttovar i),vl)
    )
    |Diagnostic_int.Conflict(i,j,vpkg) -> Some (
        Diagnostic.Conflict(from_sat (map#inttovar i),from_sat (map#inttovar j),vpkg)
    )
  )

let result map universe result = 
  let from_sat = CudfAdd.inttovar universe in
  let globalid = Cudf.universe_size universe in
  match result with
  |Diagnostic_int.Success f_int ->
      Diagnostic.Success (fun ?(all=false) () ->
        List.filter_map (function 
          |i when i = globalid -> None
          |i -> Some ({(from_sat i) with Cudf.installed = true})
        ) (f_int ~all ())
      )
  |Diagnostic_int.Failure f -> Diagnostic.Failure (fun () ->
      reason map universe (f ()))
;;

let request universe result = 
  let from_sat = CudfAdd.inttovar universe in
  match result with
  |Diagnostic_int.Sng (_,i) -> Diagnostic.Package (from_sat i)
  |Diagnostic_int.Lst (_,il) -> Diagnostic.PackageList (List.map from_sat il)
;;

(* XXX here the threatment of result and request is not uniform.
 * On one hand indexes in result must be processed with map#inttovar 
 * as they represent indexes associated with the solver.
 * On the other hand the indexes in result represent cudf uid and
 * therefore do not need to be processed.
 * Ideally the compiler should make sure that we use the correct indexes
 * but we should annotate everything making packing/unpackaing handling
 * a bit too heavy *)
let diagnosis map universe res req =
  let result = result map universe res in
  let request = request universe req in
  { Diagnostic.result = result ; request = request }

(** [univcheck ?callback universe] check all packages in the
    universe for installability 

    @return the number of packages that cannot be installed
*)
let univcheck ?(global_constraints=true) ?callback universe =
  let aux ?callback univ =
    let timer = Util.Timer.create "Algo.Depsolver.univcheck" in
    Util.Timer.start timer;
    let solver = Depsolver_int.init_solver_univ univ in
    let failed = ref 0 in
    (* This is size + 1 because we encode the global constraint of the
     * universe as a package that must be tested like any other *)
    let size = (Cudf.universe_size univ) + 1 in
    let tested = Array.make size false in
    Util.Progress.set_total Depsolver_int.progressbar_univcheck size ;
    let check = Depsolver_int.pkgcheck global_constraints callback solver tested in
    (* we do not test the last package that encodes the global constraints
     * on the universe as it is tested all the time with all other packages. *)
    for i = 0 to size - 2 do if not(check i) then incr failed done;
    Util.Timer.stop timer !failed
  in
  let map = new Depsolver_int.identity in
  match callback with
  |None -> aux universe
  |Some f ->
      let callback_int (res,req) = f (diagnosis map universe res req) in
      aux ~callback:callback_int universe
;;

(** [listcheck ?callback universe pkglist] check if a subset of packages 
    un the universe are installable.

    @param pkglist list of packages to be checked
    @return the number of packages that cannot be installed
*)
let listcheck ?(global_constraints=true) ?callback universe pkglist =
  let aux ?callback univ idlist =
    let solver = Depsolver_int.init_solver_univ univ in
    let timer = Util.Timer.create "Algo.Depsolver.listcheck" in
    Util.Timer.start timer;
    let failed = ref 0 in
    let size = (Cudf.universe_size univ) + 1 in
    let tested = Array.make size false in
    Util.Progress.set_total Depsolver_int.progressbar_univcheck size ;
    let check = Depsolver_int.pkgcheck global_constraints callback solver tested in
    List.iter (function id when id = size -> () |id -> if not(check id) then incr failed) idlist ;
    Util.Timer.stop timer !failed
  in
  let idlist = List.map (CudfAdd.vartoint universe) pkglist in
  let map = new Depsolver_int.identity in
  match callback with
  |None -> aux universe idlist
  |Some f ->
      let callback_int (res,req) = f (diagnosis map universe res req) in
      aux ~callback:callback_int universe idlist
;;

let edos_install ?(global_constraints=false) univ pkg =
  let pool = Depsolver_int.init_pool_univ ~global_constraints univ in
  let id = CudfAdd.vartoint univ pkg in
  (* globalid is a fake package indentifier used to encode global
   * constraints in the universe *)
  let globalid = Cudf.universe_size univ in
  let closure = Depsolver_int.dependency_closure_cache pool [id; globalid] in
  let solver = Depsolver_int.init_solver_closure pool closure in
  let req = 
    if global_constraints then
      Diagnostic_int.Sng (Some globalid,id) 
    else
      Diagnostic_int.Sng (None,id)
  in
  let res = Depsolver_int.solve solver req in
  diagnosis solver.Depsolver_int.map univ res req
;;

let edos_coinstall_cache global_constraints univ pool pkglist =
  let idlist = List.map (CudfAdd.vartoint univ) pkglist in
  let globalid = Cudf.universe_size univ in
  let closure = Depsolver_int.dependency_closure_cache pool (globalid::idlist) in
  let solver = Depsolver_int.init_solver_closure pool closure in
  let req =
    if global_constraints then
      Diagnostic_int.Lst (Some globalid,idlist) 
    else
      Diagnostic_int.Lst (None,idlist)
  in
  let res = Depsolver_int.solve solver req in
  diagnosis solver.Depsolver_int.map univ res req
;;

let edos_coinstall ?(global_constraints=false) univ pkglist =
  let pool = Depsolver_int.init_pool_univ ~global_constraints univ in
  edos_coinstall_cache global_constraints univ pool pkglist
;;

let edos_coinstall_prod ?(global_constraints=false) univ ll =
  let pool = Depsolver_int.init_pool_univ ~global_constraints univ in
  let return a = [a] in
  let bind m f = List.flatten (List.map f m) in
  let rec permutation = function
    |[] -> return []
    |h::t ->
        bind (permutation t) (fun t1 ->
          List.map (fun h1 -> h1 :: t1) h
        )
  in
  List.map (edos_coinstall_cache global_constraints univ pool) (permutation ll)
;;

let trim ?(global_constraints=true) universe =
  let trimmed_pkgs = ref [] in
  let callback d =
    if Diagnostic.is_solution d then
      match d.Diagnostic.request with
      |Diagnostic.Package p -> trimmed_pkgs := p::!trimmed_pkgs
      |_ -> assert false
  in
  ignore (univcheck ~global_constraints ~callback universe);
  Cudf.load_universe !trimmed_pkgs
;;

let find_broken ?(global_constraints=true) universe =
  let broken_pkgs = ref [] in
  let callback d =
    if not (Diagnostic.is_solution d) then
      match d.Diagnostic.request with
      |Diagnostic.Package p -> broken_pkgs := p::!broken_pkgs
      |_ -> assert false
  in
  ignore (univcheck ~global_constraints ~callback universe);
  !broken_pkgs
;;

let find_installable ?(global_constraints=true) universe =
  let acc = ref [] in
  let callback d =
    if Diagnostic.is_solution d then
      match d.Diagnostic.request with
      |Diagnostic.Package p -> acc := p::!acc
      |_ -> assert false
  in
  ignore (univcheck ~global_constraints ~callback universe);
  !acc
;;

let find_listbroken ?(global_constraints=true) universe pkglist =
  let broken_pkgs = ref [] in
  let callback d =
    if not (Diagnostic.is_solution d) then
      match d.Diagnostic.request with
      |Diagnostic.Package p -> broken_pkgs := p::!broken_pkgs
      |_ -> assert false
  in
  ignore (listcheck ~global_constraints ~callback universe pkglist);
  !broken_pkgs
;;

let find_listinstallable ?(global_constraints=true) universe pkglist =
  let acc = ref [] in
  let callback d =
    if Diagnostic.is_solution d then
      match d.Diagnostic.request with
      |Diagnostic.Package p -> acc := p::!acc
      |_ -> assert false
  in
  ignore (listcheck ~global_constraints ~callback universe pkglist);
  !acc
;;

let dependency_closure ?maxdepth ?conjunctive univ pkglist =
  Depsolver_int.dependency_closure ?maxdepth ?conjunctive univ pkglist

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

let output_clauses ?(global_constraints=true) ?(enc=Cnf) univ =
  let solver = Depsolver_int.init_solver_univ ~global_constraints ~buffer:true univ in
  let clauses = Depsolver_int.S.dump solver.Depsolver_int.constraints in
  let size = Cudf.universe_size univ in
  let buff = Buffer.create size in
  let globalid = size in
  let to_cnf dump =
    let str (v, p) =
      if (abs v) != globalid then
        let pkg = (CudfAdd.inttovar univ) (abs v) in
        let pol = if p then "" else "!" in
        Printf.sprintf "%s%s-%d" pol pkg.Cudf.package pkg.Cudf.version
      else ""
    in
    List.iter (fun l ->
      List.iter (fun var -> Printf.bprintf buff " %s" (str var)) l;
      Printf.bprintf buff "\n"
    ) dump
  in
  let to_dimacs dump =
    let str (v, p) =
      if v != globalid then
        if p then Printf.sprintf "%d" v else Printf.sprintf "-%d" v 
      else ""
    in
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

type solver_result =
  |Sat of (Cudf.preamble option * Cudf.universe)
  |Unsat of Diagnostic.diagnosis option
  |Error of string

(** check if a cudf request is satisfiable. we do not care about
 * universe consistency . We try to install a dummy package *)
let check_request ?cmd ?criteria ?(explain=false) (pre,universe,request) =
  let intSolver universe request =
    let deps = 
      let k =
        Cudf.fold_packages (fun acc pkg ->
          if pkg.Cudf.installed then
            match pkg.Cudf.keep with
            |`Keep_package -> (pkg.Cudf.package,None)::acc
            |`Keep_version -> (pkg.Cudf.package,Some(`Eq,pkg.Cudf.version))::acc
            |_ -> acc
          else acc
        ) [] universe
      in
      let l = request.Cudf.install @ request.Cudf.upgrade in
      debug "request consistency (keep %d) (install %d) (upgrade %d) (remove %d) (# %d)"
      (List.length k) (List.length request.Cudf.install) 
      (List.length request.Cudf.upgrade)
      (List.length request.Cudf.remove)
      (Cudf.universe_size universe);
      List.fold_left (fun acc j -> [j]::acc) (List.map (fun j -> [j]) l) k
    in
    let dummy = {
      Cudf.default_package with
      Cudf.package = "dose-dummy-request";
      version = 1;
      depends = deps;
      conflicts = request.Cudf.remove}
    in
    (* XXX it should be possible to add a package to a cudf document ! *)
    let pkglist = Cudf.get_packages universe in
    let universe = Cudf.load_universe (dummy::pkglist) in
    edos_install universe dummy
  in
  if Option.is_none cmd then begin
    let d = intSolver universe request in
    if Diagnostic.is_solution d then
      let is = Diagnostic.get_installationset d in
      Sat (Some pre,Cudf.load_universe is)
    else
      if explain then Unsat (Some d) else Unsat None
  end else begin
    let cmd = Option.get cmd in
    let criteria = if Option.is_none criteria then "-removed,-new" else Option.get criteria in
    try Sat(CudfSolver.execsolver cmd criteria (pre,universe,request)) with
    |CudfSolver.Unsat when not explain -> Unsat None
    |CudfSolver.Unsat when explain -> Unsat (Some (intSolver universe request))
    |CudfSolver.Error s -> Error s
  end
;;
