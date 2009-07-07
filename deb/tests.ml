
open IprLib
open OUnit
open Debian
open Common

open Cudf

let f_packages = "tests/Packages" ;;
let f_release = "tests/Release" ;;

let ch = Input.open_chan f_packages ;;
let ipr_list = Parse.parse_packages_in (fun x -> x) ch ;;
let _ = Debcudf.init_tables ipr_list ;;
let cudf_list = List.map Debcudf.tocudf ipr_list ;;
let universe = Cudf.load_universe cudf_list ;;

let test_format =
  "name mangling" >::: []

let test_numbering = 
  "test numbering" >::: [
    "sequence" >:: (fun _ -> 
      try
        let debconf = Cudf.lookup_package universe ("debconf",32) in
        assert_equal debconf.version 32
      with Not_found -> assert_failure "debconf version mismatch"
    );
  ] 

let test_virtual = 
  "test virtual" >::: [
    "provides" >:: (fun _ -> 
      try
        let ssmtp = Cudf.lookup_package universe ("ssmtp",1) in
        let vpkg = ("mail-transport-agent--virtual",None) in
        let provides = Cudf.who_provides universe vpkg in
        let _ = Printf.eprintf "%s\n%!" (Cudf_printer.string_of_package ssmtp) in
        let _ = List.iter (fun (pkg,_) -> Printf.eprintf "%s\n%!" (Cudf_printer.string_of_package pkg)) provides in
        assert_equal true (List.mem (ssmtp,None) provides)
      with Not_found -> assert_failure "ssmtp version mismatch"
    );
    "virtual real" >:: (fun _ -> ())
  ]

let test_conflicts =
  "test conflict" >::: [
    "self conflict" >:: (fun _ -> 
      try 
        let ssmtp = Cudf.lookup_package universe ("ssmtp",1) in
        assert_equal true (List.mem (ssmtp.package,None) ssmtp.conflicts)
      with Not_found -> assert_failure "ssmtp version mismatch"
    );
  ]

let all = 
  "all tests" >::: [ test_format ; test_numbering ; test_virtual ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
