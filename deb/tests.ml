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
open Debian
open Common
open Cudf

let f_packages = "tests/Packages" ;;
let f_release = "tests/Release" ;;

let ch = Input.open_file f_packages ;;
let ipr_list = Packages.parse_packages_in (fun x -> x) ch ;;
let tables = Debcudf.init_tables ipr_list ;;
let cudf_list = List.map (Debcudf.tocudf tables) ipr_list ;;
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
        let b = List.exists (fun (pkg,c) -> 
          if not(pkg =% ssmtp) then begin
            Printf.eprintf "%s\n%!" (Cudf_printer.string_of_package pkg);
            Printf.eprintf "%s\n%!" (Cudf_printer.string_of_package ssmtp);
            false
            end
          else 
            true
          ) provides 
        in
        assert_equal true b 
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

(*
let _ = Printf.eprintf "%s\n%!" (Cudf_printer.string_of_package ssmtp) in
let _ = List.iter (fun (pkg,Some(1)) -> Printf.eprintf "%s\n%!" (Cudf_printer.string_of_package pkg)) provides in
*)
