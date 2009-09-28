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

open OUnit
open Common

let parse_uri =
  "parse uri" >::: [
    "deb" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "deb://path/to/file" 
      in assert_equal true (protocol = "deb" && path = "path/to/file")
    ) ;
    "hdlist" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "hdlist://path/to/file" 
      in assert_equal true (protocol = "hdlist" && path = "path/to/file")
    ) ;
    "synth" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "synth://path/to/file" 
      in assert_equal true (protocol = "synth" && path = "path/to/file")
    ) ;

    "cudf" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "cudf:///path/to/file" 
      in assert_equal true (protocol = "cudf" && path = "/path/to/file")
    ) ;
    "sqlite" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "sqlite:///path/to/file" 
      in assert_equal true (protocol = "sqlite" && path = "/path/to/file")
    ) ;
    (* TODO : I know that the dbname and queryOpt are wrong *)
    "pgsql" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "pgsql://test:tester@localhost/dbname?v1=k1&v2=k2"
      in assert_equal true (
        protocol = "pgsql" && 
        userOpt = Some "test" &&
        passOpt = Some "tester" &&
        hostOpt = Some "localhost" &&
        path = "dbname" &&
        queryOpt = Some "v1=k1&v2=k2"
      )
    ) ;
  ]

(*
let test_who_conflicts =
  "who_conflict" >:: (fun _ ->
    let maps = CudfAdd.build_maps universe in
    let l = maps.CudfAdd.who_conflicts electric_engine2 in
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal engine_conflicts_set set)
  )

let test_who_provides =
  "who_provides" >:: (fun _ ->
    let maps = CudfAdd.build_maps universe in
    let l = maps.CudfAdd.who_conflicts electric_engine2 in
    let set = List.fold_right S.add l S.empty in
    assert_equal true (S.equal engine_conflicts_set set)
  )

let test_lookup_packages = 

let test_projection

let test_compare

let test_realversionmap

let test_mdf

*)


let all = 
  "all tests" >::: [
    parse_uri ;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
