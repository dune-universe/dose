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

open OUnit
open Common

let test_deb_local =
  "deb local" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "deb://Packages.gz" 
    in assert_equal true (protocol = Url.Deb && path = "Packages.gz")
  ) 
;;

let test_deb_path =
  "deb path" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "deb:///var/lib/Packages.gz" 
    in assert_equal true (protocol = Url.Deb && path = "/var/lib/Packages.gz")
  )
;;

let test_hdlist = 
  "hdlist" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "hdlist://path/to/file" 
    in assert_equal true (protocol = Url.Hdlist && path = "path/to/file")
  )
;;

let test_synth =
  "synthesis" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "synthesis://path/to/file" 
    in assert_equal true (protocol = Url.Synthesis && path = "path/to/file")
  )
;;

let test_cudf = 
  "cudf" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "cudf://path/to/file" 
    in assert_equal true (protocol = Url.Cudf && path = "path/to/file")
  )
;;

let test_sqlite =
  "sqlite" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "sqlite:///path/to/file" 
    in assert_equal true (protocol = Url.Sqlite && path = "/path/to/file")
  ) 
;;

let test_pgsql =
  "pgsql" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "pgsql://test:tester@localhost:10/dbname?query=lalalal;v2=k2"
    in assert_equal true (
      protocol = Url.Pgsql && 
      userOpt = Some "test" &&
      passOpt = Some "tester" &&
      hostOpt = Some "localhost" &&
      portOpt = Some "10" &&
      path = "dbname" &&
      queryOpt = Some "lalalal;v2=k2"
    )
  ) 
;;

let parse_uri =
  "parse uri" >::: [
    test_deb_local;
    test_deb_path;
    test_hdlist;
    test_synth;
    test_cudf;
    test_sqlite;
    test_pgsql;
  ]

(* XXX this file should not be in the algo test directory, but in a more
 * central location *)
let f_legacy = "../algo/tests/legacy.cudf"

module S = CudfAdd.Cudf_set

let (universe,request) =
  let (_,univ,request) = Cudf_parser.parse_from_file f_legacy in
  (Cudf.load_universe univ,Option.get request)

let toset l =
  List.fold_right (fun e set ->
    S.add (Cudf.lookup_package universe e) set
  ) l S.empty

let engine_provides_set = S.empty
let engine_conflicts_set = S.empty
let electric_engine2 = Cudf.lookup_package universe ("electric-engine",2) 

let test_who_conflicts =
  "who_conflict" >:: (fun _ ->
    let maps = CudfAdd.build_maps universe in
    let l = maps.CudfAdd.who_conflicts electric_engine2 in
    let engine_conflicts_set = toset 
      [ ("electric-engine",1);
        ("gasoline-engine",1);
        ("gasoline-engine",2); ]
    in
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal engine_conflicts_set set)
  )

let test_who_provides =
  "who_provides" >:: (fun _ ->
    let maps = CudfAdd.build_maps universe in
    let l = maps.CudfAdd.who_provides ("electric-engine",None) in
    let engine_provides_set = toset 
      [ ("electric-engine",1);
        ("electric-engine",2); ]
    in
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal engine_provides_set set)
  )

let test_lookup_packages =
  "look up packages" >::: [
    test_who_conflicts;
    test_who_provides;
  ]

let test_encode =
  "name mangling" >::: [
    "name encoding" >:: (fun _ ->
      assert_equal (CudfAdd.encode "/bin/bash__") "/bin/bash%5f%5f"
    )
  ]
;;


(*
let test_projection

let test_compare

let test_realversionmap

let test_mdf

*)

let all = 
  "all tests" >::: [
    parse_uri ;
    test_encode;
    test_lookup_packages;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
