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
open OUnit
open Cudf

open Common

module S = Set.Make(struct type t = Cudf.package let compare = compare end)

let f_legacy = "tests/legacy.cudf"
let f_legacy_sol = "tests/legacy-sol.cudf"
let f_dependency = "tests/dependency.cudf"
(* let f_conj_dependency = "tests/conj_dependency.cudf" *)
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
(* let conj_dependency_set = toset f_conj_dependency *)
let cone_set = toset f_cone
let engine_conflicts_set = toset f_engine_conflicts

let solver = Depsolver.load universe ;;

let test_install =
  "install" >:: (fun _ ->
    let bicycle = Cudf.lookup_package universe ("bicycle", 7) in
    let d = Depsolver.edos_install solver bicycle in
    match d.Diagnostic.result with
    |Diagnostic.Success _ -> assert_bool "pass" true
    |Diagnostic.Failure _ -> assert_failure "fail"
  )

let test_coinstall = 
  "coinstall" >:: (fun _ -> 
    let electric_engine1 = Cudf.lookup_package universe ("electric-engine",1) in
    let electric_engine2 = Cudf.lookup_package universe ("electric-engine",2) in
    let d = Depsolver.edos_coinstall solver [electric_engine1;electric_engine2] in
    match d.Diagnostic.result with
    |Diagnostic.Success f -> assert_failure "fail"
    |Diagnostic.Failure f -> assert_bool "pass" true
  )

(* debian testing 18/11/2009 *)
let test_distribcheck =
  "distribcheck" >:: (fun _ -> 
    let f_debian = "tests/debian.cudf" in
    let universe =
      let (_,pl,_) = Cudf_parser.parse_from_file f_debian in
      Cudf.load_universe pl
    in
    let i = Depsolver.univcheck universe in
    assert_equal 20 i
  ) 

let test_trim =
  "trim" >:: (fun _ ->
    let f_debian = "tests/debian.cudf" in
    let universe =
      let (_,pl,_) = Cudf_parser.parse_from_file f_debian in
      Cudf.load_universe pl
    in
    let l = Depsolver.trim universe in
    assert_equal (25606 - 20) (Cudf.universe_size l)
  )
 
let test_dependency_closure = 
  "dependency closure" >:: (fun _ -> 
    let car = Cudf.lookup_package universe ("car",1) in
    let l = Depsolver.dependency_closure universe [car] in
    (* List.iter (fun pkg -> print_endline (CudfAdd.print_package pkg)) l; *)
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal dependency_set set)
  )

let test_conjunctive_dependency_closure =
  "conjunctive dependency closure" >:: (fun _ ->
    List.iter (fun pkg ->
      let dcl = Depsolver.dependency_closure ~conjuntive:true universe [pkg] in
(*      print_endline (CudfAdd.print_package pkg);
      List.iter (fun pkg -> print_endline (CudfAdd.print_package pkg)) dcl;
      print_newline (); *)
      let d = Depsolver.edos_coinstall solver (dcl) in
      match d.Diagnostic.result with
      |Diagnostic.Success _ -> assert_bool "pass" true
      |Diagnostic.Failure _ -> Diagnostic.print ~explain:true stdout d ; assert_failure "fail"
    ) (Cudf.get_packages universe)
  )

(* blah ... *)
(* let test_dependency_closure_graph = 
  "conjunctive dependency closure" >:: (fun _ -> 
    let (_,pkglist,_) = Cudf_parser.parse_from_file f_legacy in
    let mdf = Mdf.load_from_list pkglist in
    let maps = mdf.Mdf.maps in
    let idlist = List.map maps.CudfAdd.map#vartoint pkglist in
    let graph = Strongdeps_int.conjdepgraph mdf.Mdf.index idlist in
    let car = Cudf.lookup_package universe ("bicycle",7) in
    let l = Strongdeps_int.conjdeps graph (maps.CudfAdd.map#vartoint car) in
    let l = List.map maps.CudfAdd.map#inttovar l in
    (* List.iter (fun pkg -> print_endline (CudfAdd.print_package pkg)) l; *)
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal conj_dependency_set set)
  ) *)

let test_reverse_dependencies =
  "direct reverse dependencies" >:: (fun _ ->
    let car = Cudf.lookup_package universe ("car",1) in
    let electric_engine1 = Cudf.lookup_package universe ("electric-engine",1) in
    let electric_engine2 = Cudf.lookup_package universe ("electric-engine",2) in
    let battery = Cudf.lookup_package universe ("battery",3) in
    let h = Depsolver.reverse_dependencies universe in
    let l = CudfAdd.Cudf_hashtbl.find h battery in
    let set = List.fold_right S.add l S.empty in
    let rev_dependency_set =
      List.fold_right S.add [car;electric_engine1;electric_engine2] S.empty
    in
    assert_equal true (S.equal rev_dependency_set set)
  )

let test_reverse_dependency_closure =
  "reverse dependency closure" >:: (fun _ ->
    let car = Cudf.lookup_package universe ("car",1) in
    let glass = Cudf.lookup_package universe ("glass",2) in
    let window = Cudf.lookup_package universe ("window",3) in
    let door = Cudf.lookup_package universe ("door",2) in
    let l = Depsolver.reverse_dependency_closure universe [glass] in
    let set = List.fold_right S.add l S.empty in
    let rev_dependency_set =
      List.fold_right S.add [car;glass;door;window] S.empty
    in
    assert_equal true (S.equal rev_dependency_set set)
  )

let test_depsolver =
  "depsolver" >::: [
    test_install ;
    test_coinstall ;
    test_trim ;
    test_distribcheck ;
    test_dependency_closure ;
    (* test_dependency_closure_graph ; *)
    test_reverse_dependencies ;
    test_reverse_dependency_closure ;
    test_conjunctive_dependency_closure ;
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
;;

let test_strong file l =
  let module G = Defaultgraphs.PackageGraph.G in
  let (_,universe,_) = Cudf_parser.load_from_file file in
  let g = Strongdeps.strongdeps_univ universe in
  let sdedges = G.fold_edges (fun p q l -> (p,q)::l) g [] in
  let testedges =
    List.map (fun (v,z) ->
      let p = Cudf.lookup_package universe v in
      let q = Cudf.lookup_package universe z in
      (p,q)
    ) l
  in
  if not((List.sort sdedges) = (List.sort testedges)) then
    List.iter (fun (p,q) -> 
      Printf.eprintf "%s -> %s\n" 
      (CudfAdd.print_package p)
      (CudfAdd.print_package q)
    ) sdedges
  ;
  assert_equal (List.sort sdedges) (List.sort testedges)

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
      (("cc",2),("ee",1)) ;
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
    strongdep_conj 
  ]

let test_dependency_graph =
  "syntactic dependency graph" >:: (fun _ ->
    let module SDG = Defaultgraphs.SyntacticDependencyGraph in
    let module G = SDG.G in
    let g = SDG.dependency_graph universe in
    G.iter_edges_e (fun edge ->
      print_endline (SDG.string_of_edge edge)
    ) g
  )

let test_defaultgraphs =
  "default graphs algorithms" >::: [
    (* test_dependency_graph *)
  ]

let test_cnf =
  "CNF output" >:: (fun _ ->
    let s = Depsolver.output_clauses ~enc:Depsolver.Cnf universe in
    assert_equal (String.length s) 1367
  )

let test_dimacs = 
  "DIMACS output" >:: (fun _ ->
    let s = Depsolver.output_clauses ~enc:Depsolver.Dimacs universe in
    assert_equal (String.length s) 533
  )

let test_clause_dump =
  "cnf/dimacs output" >::: [
     test_cnf ;
     test_dimacs ;
  ]

let all = 
  "all tests" >::: [
    test_depsolver ;
    test_cudfsolver ;
    test_strongdep ;
    test_defaultgraphs ;
    test_clause_dump ;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
