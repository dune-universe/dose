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
open Debian
open Common
open Cudf

let f_packages = "tests/Packages" ;;
let f_release = "tests/Release" ;;

let ch = Input.open_file f_packages ;;
let extras_properties = [
  ("maintainer", ("maintainer", `String None));
  ("size", ("size", `Nat None));
  ("installed-Size", ("installedsize", `Nat None))
];;
let extras = List.map fst extras_properties ;;
let ipr_list = Packages.parse_packages_in ~extras:extras (fun x -> x) ch ;;
let tables = Debcudf.init_tables ipr_list ;;
let cudf_list = List.map (Debcudf.tocudf ~extras:extras_properties tables) ipr_list ;;
let universe = Cudf.load_universe cudf_list ;;
let maps = CudfAdd.build_maps universe ;;

let test_format =
  "name mangling" >::: []

let test_numbering = 
  "test numbering" >::: [
    "sequence" >:: (fun _ -> 
      try
        let debconf = Cudf.lookup_package universe ("debconf",32) in
        Printf.eprintf "debconf : %s\n" (CudfAdd.string_of_package debconf);
        assert_equal debconf.version 32
      with Not_found -> assert_failure "debconf version mismatch"
    );
  ] 

let test_virtual = 
  "test virtual" >::: [
    "provides" >:: (fun _ -> 
      try
        let ssmtp = Cudf.lookup_package universe ("ssmtp",8366) in
        let vpkg = ("mail-transport-agent--virtual",None) in
        let provides = maps.CudfAdd.who_provides vpkg in
        assert_equal true (List.exists ((=%) ssmtp) provides)
      with Not_found -> assert_failure "ssmtp version mismatch"
    );
    "virtual real" >:: (fun _ -> ())
  ]

let test_conflicts =
  "test conflict" >::: [
    "self conflict" >:: (fun _ -> 
      try 
        let ssmtp = Cudf.lookup_package universe ("ssmtp",8366) in
        assert_equal true (List.mem (ssmtp.package,None) ssmtp.conflicts)
      with Not_found -> assert_failure "ssmtp version mismatch"
    );
  ]

let test_mapping =
  "test deb -> cudf mapping" >::: [
    test_format ;
(*    test_numbering ; *)
    test_virtual
  ]

let all = 
  "all tests" >::: [ test_mapping ; test_conflicts ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
