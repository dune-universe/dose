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
let cudf_dir = "tests/cudf"
let f_legacy = Filename.concat cudf_dir "legacy.cudf"

let test_deb_local =
  "deb local" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "deb://Packages.gz" 
    in assert_equal true (protocol = `Deb && path = "Packages.gz")
  ) 
;;

let test_deb_path =
  "deb path" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "deb:///var/lib/Packages.gz" 
    in assert_equal true (protocol = `Deb && path = "/var/lib/Packages.gz")
  )
;;

let test_hdlist = 
  "hdlist" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "hdlist://path/to/file" 
    in assert_equal true (protocol = `Hdlist && path = "path/to/file")
  )
;;

let test_synth =
  "synthesis" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "synthesis://path/to/file" 
    in assert_equal true (protocol = `Synthesis && path = "path/to/file")
  )
;;

let test_cudf = 
  "cudf" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "cudf://path/to/file" 
    in assert_equal true (protocol = `Cudf && path = "path/to/file")
  )
;;

let test_sqlite =
  "sqlite" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "sqlite:///path/to/file" 
    in assert_equal true (protocol = `Sqlite && path = "/path/to/file")
  ) 
;;

let test_pgsql =
  "pgsql" >:: (fun _ ->
    let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
      Input.parse_uri "pgsql://test:tester@localhost:10/dbname?query=lalalal;v2=k2"
    in assert_equal true (
      protocol = `Pgsql && 
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
  (* Some useful very long strings for testing encoding and decoding. *)
  let a_lot_of =  (* a huge number *)
    (Pcre.config_match_limit + 111)
  in
  let a_lot_of_a           = String.make a_lot_of 'a'
  (* This test takes too much time... 
  and a_lot_of_pipes       = String.make a_lot_of '|' in
  let a_lot_of_hexed_pipes = ExtLib.String.replace_chars (fun _ -> "%7c") a_lot_of_pipes *)
  in
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
    
    ("all ASCII characters",
     "\000\001\002\003\004\005\006\007\b\t\n\011\012\r\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255",
     "%00%01%02%03%04%05%06%07%08%09%0a%0b%0c%0d%0e%0f%10%11%12%13%14%15%16%17%18%19%1a%1b%1c%1d%1e%1f%20%21%22%23%24%25%26%27()%2a+%2c-./0123456789%3a%3b%3c%3d%3e%3f@ABCDEFGHIJKLMNOPQRSTUVWXYZ%5b%5c%5d%5e%5f%60abcdefghijklmnopqrstuvwxyz%7b%7c%7d%7e%7f%80%81%82%83%84%85%86%87%88%89%8a%8b%8c%8d%8e%8f%90%91%92%93%94%95%96%97%98%99%9a%9b%9c%9d%9e%9f%a0%a1%a2%a3%a4%a5%a6%a7%a8%a9%aa%ab%ac%ad%ae%af%b0%b1%b2%b3%b4%b5%b6%b7%b8%b9%ba%bb%bc%bd%be%bf%c0%c1%c2%c3%c4%c5%c6%c7%c8%c9%ca%cb%cc%cd%ce%cf%d0%d1%d2%d3%d4%d5%d6%d7%d8%d9%da%db%dc%dd%de%df%e0%e1%e2%e3%e4%e5%e6%e7%e8%e9%ea%eb%ec%ed%ee%ef%f0%f1%f2%f3%f4%f5%f6%f7%f8%f9%fa%fb%fc%fd%fe%ff");
    
    ("several characters out of range 32-126 (i.e. not usual)", "\031\127\213", "%1f%7f%d5");
    ("huge string of \"allowed\" characters", a_lot_of_a, a_lot_of_a);
    (* This test takes too much time... 
    ("huge string of \"not allowed\" characters", a_lot_of_pipes, a_lot_of_hexed_pipes); *)
    ("path", "/bin/bash__", "/bin/bash%5f%5f")
  ]
  in
  (* From each triplet we generate two test cases, one for
     the function encode and one for the function decode. *)
  let (encode_tests_cases, decode_tests_cases) =
    List.split (
      List.map (fun (test_name, decoded_string, encoded_string) -> 
          ("encoding " ^ test_name) >:: (fun _ -> 
            assert_equal 
              (CudfAdd.encode decoded_string) 
              encoded_string
              ~msg:("\ndecoded_string is        = " ^ decoded_string ^ 
                    "\nencoded_string is        = " ^ (CudfAdd.encode decoded_string) ^
                    "\nencoded_string should be = " ^ encoded_string)
              ),
          
          ("decoding " ^ test_name) >:: (fun _ ->
            assert_equal
              (CudfAdd.decode encoded_string)
              decoded_string
              ~msg:("\nencoded_string is        = " ^ encoded_string ^ 
                    "\ndecoded_string is        = " ^ (CudfAdd.decode encoded_string) ^
                    "\ndecoded_string should be = " ^ decoded_string)
              )
      ) encode_decode_triplets
    )
  in
  (* We have two test suites: one for testing encoding and one for testing decoding. *)
  ("name mangling encoding" >::: encode_tests_cases,
   "name mangling decoding" >::: decode_tests_cases)
;;

(*

  let test_realversionmap

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
