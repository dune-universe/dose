(**************************************************************************************)
(*  Copyright (C) 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2010 Mancoosi Project                                               *)
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
open Algo

let sid_pkg_list = Boilerplate.read_deb "tests/DebianPackages/sid.packages.bz2";;
let lenny_pkg_list = Boilerplate.read_deb "tests/DebianPackages/lenny.packages.bz2";;
let lenny_universe = Debian.Debcudf.load_universe lenny_pkg_list;;
let sid_universe = Debian.Debcudf.load_universe sid_pkg_list;;

let test_debian_distcheck_lenny =
  "debian_distcheck_lenny" >:: (fun _ ->
    let i = Depsolver.univcheck lenny_universe in
    assert_equal 0 i
  )

let test_debian_distcheck_sid =
  "debian_distcheck_sid" >:: (fun _ ->
    let i = Depsolver.univcheck sid_universe in
    assert_equal 143 i
  )

let test_outdated =
  "test outdated" >:: (fun _ ->
    let results = Outdated.outdated ~summary:true sid_pkg_list in
    assert_equal 0 0 
  )

let test_challenged =
  "test challenged" >:: (fun _ ->
    let clusterlist = Boilerplate.parse_vpkg "" in
    let results = Challenged.challenged ~clusterlist sid_pkg_list in
    assert_equal 0 0 
  )

let test_distcheck =
  "distcheck" >::: [
    test_debian_distcheck_lenny;
    test_debian_distcheck_sid;
  ]

let all =
  "all tests" >::: [
    test_distcheck ;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()

