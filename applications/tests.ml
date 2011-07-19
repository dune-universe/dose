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

let test_debian_distcheck_lenny =
  "debian_distcheck_lenny" >:: (fun _ ->
    let ch = Input.open_file "applications/tests/lenny.packages" in
    let l = Debian.Packages.parse_packages_in ch in
    Input.close_ch ch;
    let universe = Debian.Debcudf.load_universe l in
    let i = Depsolver.univcheck universe in
    assert_equal 0 i
  )

let test_debian_distcheck_sid =
  "debian_distcheck_sid" >:: (fun _ ->
    let ch = Input.open_file "applications/tests/sid.packages" in
    let l = Debian.Packages.parse_packages_in ch in
    Input.close_ch ch;
    let universe = Debian.Debcudf.load_universe l in
    let i = Depsolver.univcheck universe in
    assert_equal 143 i
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

