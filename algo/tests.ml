
open OUnit
open IprLib
open Cudf

open Algo
module S = Set.Make(struct type t = Cudf.package let compare = compare end)

let f_legacy = "tests/legacy.cudf"
let f_dependency = "tests/dependency.cudf"

let universe =
  let (univ,_) = Cudf_parser.parse_from_file f_legacy in
  Cudf.load_universe univ

let dependency_set =
  let (pl,_) = Cudf_parser.parse_from_file f_dependency in
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
      let (pl,_) = Cudf_parser.parse_from_file f_debian in
      Cudf.load_universe pl
    in
    let solver = Depsolver.init universe in
    Depsolver.distribcheck solver
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
    (* test_distribcheck ; *)
    test_dependency_closure ;
  ]

open Cudfsolver


let all = 
  "all tests" >::: [ test_depsolver ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
