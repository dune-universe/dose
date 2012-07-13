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
open Common

module S = CudfAdd.Cudf_set

let test_dir = "tests/algo"
let cudf_dir = "tests/cudf"

let f_dependency = Filename.concat test_dir "dependency.cudf"
let f_conj_dependency = Filename.concat test_dir "conj_dependency.cudf"
let f_cone = Filename.concat test_dir "cone.cudf"
let f_engine_conflicts = Filename.concat test_dir "engine-conflicts.cudf"

let f_strongdeps_simple = Filename.concat test_dir "strongdep-simple.cudf"
let f_strongdeps_conflict = Filename.concat test_dir "strongdep-conflict.cudf"
let f_strongdeps_cycle = Filename.concat test_dir "strongdep-cycle.cudf"
let f_strongdeps_conj = Filename.concat test_dir "strongdep-conj.cudf"

let f_strongcfl_simple = Filename.concat test_dir "strongcfl-simple.cudf"
let f_strongcfl_triangle = Filename.concat test_dir "strongcfl-triangle.cudf"

let f_selfprovide = Filename.concat test_dir "selfprovide.cudf"
let f_coinst = Filename.concat test_dir "coinst.cudf"

let f_legacy = Filename.concat cudf_dir "legacy.cudf"
let f_legacy_sol = Filename.concat cudf_dir "legacy-sol.cudf"
let f_debian = Filename.concat cudf_dir "debian.cudf" 

let f_dominators_order = Filename.concat test_dir "dominators_order.cudf"
let f_dominators_cycle = Filename.concat test_dir "dominators_cycle.cudf"

let load_univ f =
  let (_,univ,_) = Cudf_parser.load_from_file f in
  univ

let (universe,request) =
  let (_,univ,request) = Cudf_parser.load_from_file f_legacy in
  (univ,Option.get request)

let universe_debian = load_univ f_debian

let toset f = 
  let (_,pl,_) = Cudf_parser.parse_from_file f in
  List.fold_right S.add pl S.empty

let cone_set = toset f_cone
let engine_conflicts_set = toset f_engine_conflicts

let solver = Depsolver.load universe ;;

let test_install =
  "install" >:: (fun _ ->
    let bicycle = Cudf.lookup_package universe ("bicycle", 7) in
    let d = Depsolver.edos_install universe bicycle in
    Diagnostic.printf d;
    match d.Diagnostic.result with
    |Diagnostic.Success _ -> assert_bool "pass" true
    |Diagnostic.Failure _ -> assert_failure "fail"
  )

let test_coinst_legacy = 
  "coinstall legacy" >:: (fun _ -> 
    let electric_engine1 = Cudf.lookup_package universe ("electric-engine",1) in
    let electric_engine2 = Cudf.lookup_package universe ("electric-engine",2) in
    let d = Depsolver.edos_coinstall universe [electric_engine1;electric_engine2] in
    match d.Diagnostic.result with
    |Diagnostic.Success f -> assert_failure "fail"
    |Diagnostic.Failure f -> assert_bool "pass" true
  )

let test_coinst_real = 
  "coinst debian" >:: (fun _ -> 
    let exim4 = Cudf.lookup_package universe_debian ("exim4",1) in
    let sendmail = Cudf.lookup_package universe_debian ("sendmail",3) in
    let d = Depsolver.edos_coinstall universe_debian [exim4;sendmail] in
    match d.Diagnostic.result with
    |Diagnostic.Success f -> assert_failure "fail"
    |Diagnostic.Failure f -> assert_bool "pass" true 
  )

let test_coinst_prod = 
  "coinst product" >:: (fun _ -> 
    let universe = load_univ f_coinst in
    let al = Cudf.lookup_packages universe "a" in
    let bl = Cudf.lookup_packages universe "b" in
    let a1 = Cudf.lookup_package universe ("a",1) in
    let a2 = Cudf.lookup_package universe ("a",2) in
    let b1 = Cudf.lookup_package universe ("b",1) in
    let b2 = Cudf.lookup_package universe ("b",2) in
    let res_expected = [
      List.sort [a1;b1],false;
      List.sort [a1;b2],true;
      List.sort [a2;b1],true;
      List.sort [a2;b2],false
      ]
    in
    let dl = Depsolver.edos_coinstall_prod universe [al;bl] in
    let resl =
      List.map (fun res ->
        let l =
          match res.Diagnostic.request with
          |Diagnostic.PackageList l -> List.sort l
          |_ -> []
        in
        let r =
          match res.Diagnostic.result with
          |Diagnostic.Success _ -> true
          |Diagnostic.Failure _ -> false
        in
        (l,r)
      ) dl
    in
    assert_equal (List.sort resl) (List.sort res_expected)
  )

let test_essential_broken =
  "essential broken" >:: (fun _ -> 
    let pkg1 = { Cudf.default_package with 
      Cudf.package = "a";
      Cudf.depends = [[("b",None)]];
      Cudf.keep = `Keep_package;
    } in
    let pkg2 = { Cudf.default_package with 
      Cudf.package = "c";
    } in
    let universe = Cudf.load_universe [pkg1;pkg2] in
    let d = Depsolver.edos_install ~global_constraints:true universe pkg2 in
    match d.Diagnostic.result with
    |Diagnostic.Success _ -> assert_failure "fail"
    |Diagnostic.Failure _ -> assert_bool "pass" true
  ) 

let test_essential_multi =
  "essential multi" >:: (fun _ -> 
    let pkg1a = { Cudf.default_package with 
      Cudf.package = "a";
      version = 1;
      installed = true;
      keep = `Keep_package;
    } in
    let pkg1b = { Cudf.default_package with 
      Cudf.package = "a";
      version = 2;
      conflicts = [("c",None)];
      keep = `Keep_package;
    } in
    let pkg2 = { Cudf.default_package with 
      Cudf.package = "c";
    } in
    let universe = Cudf.load_universe [pkg1a;pkg1b;pkg2] in
    let d = Depsolver.edos_install ~global_constraints:true universe pkg2 in
    match d.Diagnostic.result with
    |Diagnostic.Failure _ -> assert_failure "fail"
    |Diagnostic.Success _ -> assert_bool "pass" true
  ) 


(* debian testing 18/11/2009 *)
let test_distribcheck =
  "distribcheck" >:: (fun _ -> 
    let i = Depsolver.univcheck universe_debian in
    assert_equal 20 i
  ) 

let test_trim =
  "trim" >:: (fun _ ->
    let l = Depsolver.trim universe_debian in
    assert_equal (25606 - 20) (Cudf.universe_size l)
  )
 
(* check if a package the depends and provides a feature is always installable *)
let test_selfprovide =
  "self provide" >:: (fun _ -> 
    let universe = load_univ f_selfprovide in
    let i = Depsolver.univcheck universe in
    assert_equal 0 i
  ) 

let test_dependency_closure = 
  "dependency closure" >:: (fun _ -> 
    let dependency_set = toset f_dependency in
    let car = Cudf.lookup_package universe ("car",1) in
    let l = Depsolver.dependency_closure universe [car] in
    (* List.iter (fun pkg -> print_endline (CudfAdd.print_package pkg)) l; *)
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal dependency_set set)
  )

let test_conjunctive_dependency_closure =
  "dependency closure conjunctive" >:: (fun _ ->
    List.iter (fun pkg ->
      let dcl = Depsolver.dependency_closure ~conjunctive:true universe [pkg] in
(*      print_endline (CudfAdd.print_package pkg);
      List.iter (fun pkg -> print_endline (CudfAdd.print_package pkg)) dcl;
      print_newline (); *)
      let d = Depsolver.edos_coinstall universe dcl in
      match d.Diagnostic.result with
      |Diagnostic.Success _ -> assert_bool "pass" true
      |Diagnostic.Failure _ ->
          (* let msg = Diagnostic.fprintf ~explain:true Format.str_formatter d
           * in *)
          assert_equal false true
    ) (Cudf.get_packages universe)
  )

let test_conj_dependency = 
  "conjunctive dependency closure" >:: (fun _ -> 
    let conj_dependency_set = toset f_conj_dependency in
    let pkg = Cudf.lookup_package universe ("bicycle",7) in
    let g = Strongdeps.conjdeps universe [pkg] in
    let l = Defaultgraphs.PackageGraph.conjdeps g pkg in
    (*
    List.iter (fun pkg ->
      print_endline (CudfAdd.string_of_package pkg)
      ) l;
    *)
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal conj_dependency_set set)
  )

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

(* XXX this is the same function in Depsolver.conv that is
 * not exposed in the mli *) 
let conv solver = function
  |Depsolver_int.Success(f_int) ->
      Diagnostic_int.Success(fun ?all () ->
        List.map solver.Depsolver_int.map#inttovar (f_int ())
      )
  |Depsolver_int.Failure(r) -> Diagnostic_int.Failure(r)
;;

(*
let test_ =
  "" >:: (fun _ ->
    let univ = universe_debian in
    let pool = Depsolver_int.init_pool_univ univ in
    let id = in
    let idlist = List.map (CudfAdd.vartoint univ) pkglist in
    let closure = Depsolver_int.dependency_closure_cache pool idlist in
    let solver = Depsolver_int.init_solver_closure pool closure in
    let req = Diagnostic_int.Sng id in
    match conv solver (Depsolver_int.solve solver req) with
    |Diagnostic.Success _ -> assert_bool "pass" true
    |Diagnostic.Failure _ -> assert_failure "fail"
  )
*)

let test_depsolver =
  "depsolver" >::: [
    test_install ;
    test_coinst_real ;
    test_coinst_legacy ;
    test_coinst_prod ;
    test_trim ;
    test_essential_broken ;
    test_essential_multi ;
    test_distribcheck ;
    test_selfprovide ;
    test_dependency_closure ;
    test_conj_dependency ;
    test_reverse_dependencies ;
    test_reverse_dependency_closure ;
    test_conjunctive_dependency_closure ;
  ]

let solution_set =
  let (_,pl,_) = Cudf_parser.parse_from_file f_legacy_sol in
  List.fold_right S.add pl S.empty

let test_strong ?(transitive=true) file l =
  let module G = Defaultgraphs.PackageGraph.G in
  let (_,universe,_) = Cudf_parser.load_from_file file in
  let g = Strongdeps.strongdeps_univ ~transitive universe in
  let sdedges = G.fold_edges (fun p q l -> (p,q)::l) g [] in
  let testedges =
    List.map (fun (v,z) ->
      let p = Cudf.lookup_package universe v in
      let q = Cudf.lookup_package universe z in
      (p,q)
    ) l
  in
  (*
  if not((List.sort sdedges) = (List.sort testedges)) then
    List.iter (fun (p,q) -> 
      Printf.eprintf "%s -> %s\n" 
      (CudfAdd.string_of_package p)
      (CudfAdd.string_of_package q)
    ) sdedges
  ;
  *)
  assert_equal (List.sort sdedges) (List.sort testedges)

let test_strongcfl file l =
  let universe = load_univ file in
  let module SG = Strongconflicts.CG in
  let g = Strongconflicts.strongconflicts universe in
  let scedges = 
    SG.fold_edges (fun p q l ->
      (p, q)::l
    ) g [] 
  in
  let testedges =
    List.map (fun (v,z) ->
      let p = Cudf.lookup_package universe v in
      let q = Cudf.lookup_package universe z in
      (p,q)
    ) l
  in
  (*
  if not((List.sort scedges) = (List.sort testedges)) then
    List.iter (fun (p,q) -> 
      Printf.eprintf "%s <-> %s\n" 
      (CudfAdd.string_of_package p)
      (CudfAdd.string_of_package q)
    ) scedges
  ;
  *)
  assert_equal (List.sort scedges) (List.sort testedges)

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
      (("aa",1),("bb",1)) ;
      (("aa",1),("dd",1)) ;
      (("aa",1),("ee",1)) ;
      (("aa",1),("ff",1)) ;
      (("bb",1),("ee",1)) ;
      (("bb",1),("ff",1)) ;
      (("cc",1),("ee",1)) ;
      (("cc",1),("ff",1)) ;
      (("ee",1),("ff",1)) ]
    in
    test_strong f_strongdeps_conj edge_list
  )

(* This test is no longer true.
 * transitive = false does not mean that the result is
 * the transitive reduction of the strong dependency
 * graph *)
let strongdep_detrans =
  "strongdep detrans" >:: (fun _ ->
    let edge_list = [
      (("aa",1),("bb",1)) ;
      (("aa",1),("dd",1)) ;
      (("bb",1),("ee",1)) ;
      (("cc",1),("ee",1)) ;
      (("ee",1),("ff",1)) ]
    in
    test_strong ~transitive:false f_strongdeps_conj edge_list
  )

let strongcfl_simple = 
  "strongcfl simple" >:: (fun _ ->
    let edge_list = [
      (("bravo", 1), ("alpha", 1))  ;
      (("quebec", 1), ("alpha", 1)) ;
      (("papa", 1), ("bravo", 1))   ;
      (("quebec", 1), ("papa", 1))  ]
    in
    test_strongcfl f_strongcfl_simple edge_list
  )

let strongcfl_triangle = 
  "strongcfl triangle" >:: (fun _ ->
    let edge_list = [
      (("romeo", 1), ("quebec", 1)) ]
    in
    test_strongcfl f_strongcfl_triangle edge_list
  )

let test_strongdep =
  "strong dependencies" >::: [
    strongdep_simple ;
    strongdep_conflict ;
    strongdep_cycle ;
    strongdep_conj ;
  ]

let test_strongcfl = 
  "strong conflicts" >::: [
    strongcfl_simple ;
    strongcfl_triangle
  ]   

let test_dependency_graph =
  "syntactic dependency graph" >:: (fun _ ->
    (*
    let module SDG = Defaultgraphs.SyntacticDependencyGraph in
    let module G = SDG.G in
    let g = SDG.dependency_graph universe in
    G.iter_edges_e (fun edge ->
      print_endline (SDG.string_of_edge edge)
    ) g
    *)
    ()
  )

let test_dominators_tarjan_order =
  "dominators tarjan order" >:: (fun _ ->
    let universe = load_univ f_dominators_order in
    let g = Strongdeps.strongdeps_univ ~transitive:false universe in
    let dg = Dominators.dominators_tarjan g in
    let edges = ["quebec","romeo"] in
    let edges_packages = 
      List.map (fun (p,q) -> 
        Cudf.lookup_package universe (p,1),
        Cudf.lookup_package universe (q,1)
      ) edges
    in
    let size = Defaultgraphs.PackageGraph.G.nb_edges dg in
    let all = 
      List.map (fun (p,q) ->
        Defaultgraphs.PackageGraph.G.find_all_edges dg p q
      ) edges_packages 
    in
    List.iter (fun l ->
      assert_equal ~msg:"too many edges" (List.length l) 1
    ) all ;
    assert_equal ~msg:"too many edges in the graph" size 1
  )

let test_dominators_tarjan_cycle =
  "dominators tarjan cycle" >:: (fun _ ->
    let universe = load_univ f_dominators_cycle in
    let g = Strongdeps.strongdeps_univ ~transitive:false universe in
    let dg = Dominators.dominators_tarjan g in
    let dom_pkglist = Defaultgraphs.PackageGraph.G.fold_vertex (fun v acc -> v::acc) dg [] in
    let dom_univ = Cudf.load_universe dom_pkglist in
    let edges = [
      ("a1",1),("a2",1);
      ("a1",1),("a3",1);
      ("a1",1),("a7",1);
      ("a3",1),("a4/a6",1);
      ("a4/a6",1),("a5",1) 
      ]
    in
    let edges_packages = 
      List.map (fun (p,q) -> 
        try
          Cudf.lookup_package dom_univ p,
          Cudf.lookup_package dom_univ q
        with Not_found -> failwith (Printf.sprintf "(%s,%s) not found" (fst p) (fst q))
      ) edges
    in
    let size = Defaultgraphs.PackageGraph.G.nb_edges dg in
    let all = 
      List.map (fun (p,q) ->
        Defaultgraphs.PackageGraph.G.find_all_edges dg p q
      ) edges_packages 
    in
    List.iter (fun l ->
      assert_equal ~msg:"too many edges" (List.length l) 1
    ) all ;
    assert_equal ~msg:"too many edges in the graph" size (List.length edges)
  )

let test_dominators_tarjan_legacy =
  "dominators tarjan legacy" >:: (fun _ ->
    let universe = load_univ f_legacy in
    let g = Strongdeps.strongdeps_univ ~transitive:false universe in
    let dg = Dominators.dominators_tarjan g in
    let dom_pkglist = Defaultgraphs.PackageGraph.G.fold_vertex (fun v acc -> v::acc) dg [] in
    let dom_univ = Cudf.load_universe dom_pkglist in
    let edges = [
      ("bicycle/user",1),("bike-tire",1);
      ("bicycle/user",1),("pedal",1);
      ("bicycle/user",1),("seat",1);
      ("car",1),("battery",3);
      ("gasoline-engine",1),("turbo",1);
      ("window",3),("glass",2);
      ("window",2),("glass",1);
      ("bike-tire",1),("rim",1);
      ] 
    in
    let edges_packages = 
      List.map (fun (p,q) -> 
        try
          Cudf.lookup_package dom_univ p,
          Cudf.lookup_package dom_univ q
        with Not_found -> failwith (Printf.sprintf "(%s,%s) not found" (fst p) (fst q))
      ) edges
    in
    let size = Defaultgraphs.PackageGraph.G.nb_edges dg in
    let all = 
      List.map (fun (p,q) ->
        Defaultgraphs.PackageGraph.G.find_all_edges dg p q
      ) edges_packages 
    in
    List.iter (fun l ->
      assert_equal ~msg:"too many edges" (List.length l) 1
    ) all ;
    assert_equal ~msg:"too many edges in the graph" size (List.length edges)
  )


let test_dominators_direct_order =
  "dominators direct" >:: (fun _ ->
    let universe = load_univ f_dominators_order in
    let g = Strongdeps.strongdeps_univ ~transitive:false universe in
    let dg = Dominators.dominators_direct g in
    let romeo = Cudf.lookup_package universe ("romeo",1) in
    let quebec = Cudf.lookup_package universe ("quebec",1) in
    let size = Defaultgraphs.PackageGraph.G.nb_edges dg in
    let all = Defaultgraphs.PackageGraph.G.find_all_edges dg quebec romeo in
    assert_equal ~msg:"too many edges between quebec and romeo" (List.length all) 1;
    assert_equal ~msg:"too many edges in the graph" size 1
  )

let test_dominators =
  "dominators algorithms" >::: [
    test_dominators_tarjan_order;
    test_dominators_tarjan_cycle;
    test_dominators_tarjan_legacy;
    test_dominators_direct_order;
  ]

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
     (* XXX remove this test for the moment... it should be checked *)
    (* test_dimacs ; *)
  ]

let all = 
  "all tests" >::: [
    test_depsolver ;
    test_strongdep ;
    test_strongcfl ;
    test_defaultgraphs ;
    test_dominators
    (* test_clause_dump ; *)
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
