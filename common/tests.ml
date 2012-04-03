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

let test_dir = "tests/common"

(* XXX this file should not be in the algo test directory, but in a more
 * central location *)
let f_legacy = "tests/algo/legacy.cudf"

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
    (* test_pgsql; *)
  ]

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
    let conflicts = CudfAdd.init_conflicts universe in
    let l = CudfAdd.who_conflicts conflicts universe electric_engine2 in
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
    let l = CudfAdd.who_provides universe ("electric-engine",None) in
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

let (test_encode, test_decode) =
  (* List of triplets: (test_name, decoded_string, encoded_string) *)
  let encode_decode_triplets = [
    ("empty", "", "");
    ("single \"allowed\" character", "a", "a");
    ("single \"not allowed\" character", "|", "%7c");
    ("single percent character", "%", "%25");
    ("several \"allowed\" characters", "abcdef", "abcdef");
    ("several \"not allowed\" characters", "[_|?]", "%5b%5f%7c%3f%5d");
    ("several percent characters", "%%%%%%", "%25%25%25%25%25%25");
    ("several mixed characters", "a[b_c|d?e]f", "a%5bb%5fc%7cd%3fe%5df");
    ("all ASCII characters in range 32-126 (i.e. normal)",
     " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~",
     "%20%21%22%23%24%25%26%27()%2a+%2c-./0123456789%3a%3b%3c%3d%3e%3f@ABCDEFGHIJKLMNOPQRSTUVWXYZ%5b%5c%5d%5e%5f%60abcdefghijklmnopqrstuvwxyz%7b%7c%7d%7e");
    ("several characters out of range 32-126 (i.e. not usual)", "\031\127\213", "%1f%7f%d5");
    ("path", "/bin/bash__", "/bin/bash%5f%5f");
    ("huge string of \"allowed\" characters",
     String.make (Pcre.config_match_limit + 111) 'a',
     String.make (Pcre.config_match_limit + 111) 'a')
    (* TODO: huge string of not allowed characters. *)
  ]
  in
  (* From each triplet we generate two test cases, one for
     the function encode and one for the function decode. *)
  let (encode_tests_cases, decode_tests_cases)=
    List.split (
    List.map
      (fun (test_name, decoded_string, encoded_string) -> 
	(
	("encoding " ^ test_name) >::
	(fun _ -> 
	  assert_equal 
	    (CudfAdd.encode decoded_string) 
	    encoded_string
	    ~msg:("\ndecoded_string is        = " ^ decoded_string ^ 
		  "\nencoded_string is        = " ^ (CudfAdd.encode decoded_string) ^
		  "\nencoded_string should be = " ^ encoded_string)
	    ),
	
	("decoding " ^ test_name) >::
	(fun _ ->
	  assert_equal
	    (CudfAdd.decode encoded_string)
	    decoded_string
	    ~msg:("\nencoded_string is        = " ^ encoded_string ^ 
		  "\ndecoded_string is        = " ^ (CudfAdd.decode encoded_string) ^
		  "\ndecoded_string should be = " ^ decoded_string)
	    )
	  ))
      encode_decode_triplets
      )
  in
  (* We have two test suites: one for testing encoding and one for testing decoding. *)
  ("name mangling encoding" >::: encode_tests_cases,
   "name mangling decoding" >::: decode_tests_cases)

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
    test_decode;
    test_lookup_packages;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
