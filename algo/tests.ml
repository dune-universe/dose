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

open Algo
module S = Set.Make(struct type t = Cudf.package let compare = compare end)

let f_legacy = "tests/legacy.cudf"
let f_legacy_sol = "tests/legacy-sol.cudf"
let f_dependency = "tests/dependency.cudf"

let (universe,request) =
  let (_,univ,request) = Cudf_parser.parse_from_file f_legacy in
  (Cudf.load_universe univ,Option.get request)

let dependency_set =
  let (_,pl,_) = Cudf_parser.parse_from_file f_dependency in
  List.fold_right S.add pl S.empty

let bicycle = Cudf.lookup_package universe ("bicycle", 7)
let car = Cudf.lookup_package universe ("car",1)
let electric_engine1 = Cudf.lookup_package universe ("electric-engine",1)
let electric_engine2 = Cudf.lookup_package universe ("electric-engine",2)

let solver = Depsolver.init universe ;;

let test_install =
  "install" >:: (fun _ ->
    let d = Depsolver.edos_install solver bicycle in
    match d.Diagnostic.result with
    |Diagnostic.Success f -> assert_bool "pass" true
    |Diagnostic.Failure f -> assert_failure "fail"
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
    let solver = Depsolver.init universe in
    let i = Depsolver.distribcheck solver in
    assert_equal 425 i
  ) 

let test_dependency_closure = 
  "dependency closure" >:: (fun _ -> 
    let l = Depsolver.dependency_closure universe [car] in
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

let solver = Cudfsolver.init universe request ;;

let solve_same_legacy =
  "solve legacy (same solution)" >:: (fun _ ->
    let d = Cudfsolver.solve solver in
    match d.Diagnostic.result with
    |Diagnostic.Success f -> (
        let set = List.fold_right S.add (f ()) S.empty in
        assert_equal true (S.equal solution_set set)
    )
    |Diagnostic.Failure f -> assert_failure "fail"
  )

let solve_any_legacy =
  "solve legacy (any solution)" >:: (fun _ ->
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
    solve_same_legacy ;
    solve_any_legacy
  ]

let all = 
  "all tests" >::: [ test_depsolver ; test_cudfsolver ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
