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
open OUnit
open Cudf

open Common

module S = Set.Make(struct type t = Cudf.package let compare = compare end)

let f_legacy = "tests/legacy.cudf"
let f_legacy_sol = "tests/legacy-sol.cudf"
let f_dependency = "tests/dependency.cudf"
let f_cone = "tests/cone.cudf"
let f_engine_conflicts = "tests/engine-conflicts.cudf"
let f_strongdeps_simple = "tests/strongdep-simple.cudf"
let f_strongdeps_conflict = "tests/strongdep-conflict.cudf"
let f_strongdeps_cycle = "tests/strongdep-cycle.cudf"
let f_strongdeps_conj = "tests/strongdep-conj.cudf"

let (universe,request) =
  let (_,univ,request) = Cudf_parser.parse_from_file f_legacy in
  (Cudf.load_universe univ,Option.get request)


let toset f = 
  let (_,pl,_) = Cudf_parser.parse_from_file f in
  List.fold_right S.add pl S.empty

let dependency_set = toset f_dependency
let cone_set = toset f_cone
let engine_conflicts_set = toset f_engine_conflicts

let bicycle = Cudf.lookup_package universe ("bicycle", 7)
let car = Cudf.lookup_package universe ("car",1)
let electric_engine1 = Cudf.lookup_package universe ("electric-engine",1)
let electric_engine2 = Cudf.lookup_package universe ("electric-engine",2)

let solver = Depsolver.load universe ;;

let test_install =
  "install" >:: (fun _ ->
    let d = Depsolver.edos_install solver bicycle in
    match d.Diagnostic.result with
    |Diagnostic.Success _ -> assert_bool "pass" true
    |Diagnostic.Failure _ -> assert_failure "fail"
  )

let test_coinstall = 
  "coinstall" >:: (fun _ -> 
    let d = Depsolver.edos_coinstall solver [electric_engine1;electric_engine2] in
    match d.Diagnostic.result with
    |Diagnostic.Success f -> assert_failure "fail"
    |Diagnostic.Failure f -> assert_bool "pass" true
  )

let test_distribcheck =
  "distribcheck" >:: (fun _ -> 
    let f_debian = "tests/debian.cudf" in
    let universe =
      let (_,pl,_) = Cudf_parser.parse_from_file f_debian in
      Cudf.load_universe pl
    in
    let solver = Depsolver.load universe in
    let i = Depsolver.univcheck solver in
    assert_equal 425 i
  ) 

let test_dependency_closure = 
  "dependency closure" >:: (fun _ -> 
    let l = Depsolver.dependency_closure universe [car] in
    (* List.iter (fun pkg -> print_endline (CudfAdd.print_package pkg)) l; *)
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal dependency_set set)
  )

let test_depsolver =
  "depsolver" >::: [
    test_install ;
    test_coinstall ;
    test_distribcheck ;
    test_dependency_closure ;
  ]

let solution_set =
  let (_,pl,_) = Cudf_parser.parse_from_file f_legacy_sol in
  List.fold_right S.add pl S.empty

let solve_same_legacy =
  "solve legacy (same solution)" >:: (fun _ ->
    let solver = Cudfsolver.load universe request in
    let d = Cudfsolver.solve solver in
    match d.Diagnostic.result with
    |Diagnostic.Success f ->
        let set = List.fold_right S.add (f ()) S.empty in
        assert_equal true (S.equal solution_set set)
    |Diagnostic.Failure f -> assert_failure "fail"
  )

let solve_any_legacy =
  "solve legacy (any solution)" >:: (fun _ ->
    let solver = Cudfsolver.load universe request in
    let d = Cudfsolver.solve solver in
    match d.Diagnostic.result with
    |Diagnostic.Success f ->
        let sol = Cudf.load_universe (f ()) in
        let cudf = (universe,request) in
        assert_equal (true,[]) (Cudf_checker.is_solution cudf sol) 
    |Diagnostic.Failure f -> assert_failure "fail"
  )

let test_cudfsolver = 
  "cudf solver" >::: [
    solve_any_legacy ;
    solve_same_legacy ;
  ]

let test_strong file edge_list =
  let (_,pkglist,_) = Cudf_parser.parse_from_file file in
  let u = Cudf.load_universe pkglist in
  let maps = CudfAdd.build_maps u in
  let module IntGraph = Defaultgraphs.IntGraph(struct
    let pr i = CudfAdd.print_package (maps.CudfAdd.map#inttovar i)
    end)
  in
  let module G = IntGraph.G in
  let module StrongDep = Strongdeps.Make(G) in
  let g = StrongDep.strongdeps pkglist in
  let l =
    List.map (fun (v,z) ->
      let p = maps.CudfAdd.map#vartoint (Cudf.lookup_package u v) in
      let q = maps.CudfAdd.map#vartoint (Cudf.lookup_package u z) in
      (p,q)
    ) edge_list
  in
  G.iter_edges (fun v z ->
    if not(List.exists (fun (p,q) -> (v = p) && (z = q)) l)
    then assert_failure "fail"
  ) g
  ;
  List.iter (fun (v,z) ->
    if not(G.mem_edge g v z)
    then assert_failure "fail" 
  ) l

let strongdep_simple =
  "strongdep simple" >:: (fun _ ->
    let edge_list = [
      (("cc",1),("ee",1)) ;
      (("aa",1),("ee",1)) ;
      (("aa",1),("dd",1)) ;
      (("bb",1),("ee",1)) ]
    in
    test_strong f_strongdeps_simple edge_list
  )

let strongdep_conflict =
  "strongdep conflict" >:: (fun _ ->
    let edge_list = [
      (("cc",1),("ee",1)) ;
      (("aa",1),("bb",1)) ;
      (("aa",1),("ee",1)) ;
      (("aa",1),("dd",1)) ;
      (("bb",1),("ee",1)) ]
    in
    test_strong f_strongdeps_conflict edge_list
  )

let strongdep_cycle =
  "strongdep cycle" >:: (fun _ ->
    let edge_list = [
      (("bb",1),("aa",1)) ]
    in
    test_strong f_strongdeps_cycle edge_list
  )

let strongdep_conj =
  "strongdep conj" >:: (fun _ ->
    let edge_list = [
      (("cc",1),("ff",1)) ;
      (("cc",1),("ee",1)) ;
      (("ee",1),("ff",1)) ;
      (("aa",1),("bb",1)) ;
      (("aa",1),("ee",1)) ;
      (("aa",1),("ff",1)) ;
      (("aa",1),("dd",1)) ;
      (("bb",1),("ff",1)) ;
      (("bb",1),("ee",1)) ]
    in
    test_strong f_strongdeps_conj edge_list
  )


let test_strongdep =
  "strong dependencies" >::: [
    strongdep_simple ;
    strongdep_conflict ;
    strongdep_cycle ;
  (*  strongdep_conj  XXX *)
  ]

let all = 
  "all tests" >::: [
    test_depsolver ;
    test_cudfsolver ;
    test_strongdep;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
