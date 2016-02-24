open OUnit

let returns_result ?(printer=(fun _ -> "(FIXME)")) function_to_test expected_result =
  (fun args () -> assert_equal ~printer (function_to_test args) expected_result)
and raises_failure function_to_test failure_text =
  (fun args () -> assert_raises (Failure failure_text) (fun () -> function_to_test args) )


(* Test for the semantic versioning *)
open SemverNode
let get_major x = x.major
let get_minor x = x.minor
let get_patch x = x.patch

let printer x = string_of_int x

let test_parse_version_major =
  let function_to_test (v,full) = get_major (parse_version full v) in
  let returns = returns_result ~printer function_to_test in
  returns, [
    ("1.2.3",true,1);
    (" 1.2.3 ",true,1);
    (" 2.2.3-4 ",true,2);
    (" 3.2.3-pre ",true,3);
    ("v5.2.3",true,5);
    (" v8.2.3 ",true,8);
    ("\t13.2.3",true,13);
    ("=21.2.3",false,21);
    ("v=34.2.3",false,34)
  ]
    
let test_parse_version_minor =
  let function_to_test (v,full) = get_minor (parse_version full v) in
  let returns = returns_result ~printer function_to_test in
  returns, [
    ("1.1.3",true, 1);
    (" 1.1.3 ",true, 1);
    (" 1.2.3-4 ",true, 2);
    (" 1.3.3-pre ",true, 3);
    ("v1.5.3",true, 5);
    (" v1.8.3 ",true, 8);
    ("\t1.13.3",true, 13);
    ("=1.21.3",false, 21);
    ("v=1.34.3",false, 34)
  ]

let test_parse_version_patch =
  let function_to_test (v,full) = get_patch (parse_version full v) in
  let returns = returns_result ~printer function_to_test in
  returns, [
    ("1.2.1",true, 1);
    (" 1.2.1 ",true, 1);
    (" 1.2.2-4 ",true, 2);
    (" 1.2.3-pre ",true, 3);
    ("v1.2.5",true, 5);
    (" v1.2.8 ",true, 8);
    ("\t1.2.13",true, 13);
    ("=1.2.21",false, 21);
    ("v=1.2.34",false, 34)
  ]

let make_test_cases_parse (assert_function,triplets) =
  List.map ( fun (v,full,result) -> 
    v >:: assert_function result (v,full)
  ) triplets

let test_parse_and_compare_gt =
  let function_to_test (v1,v2,full) = parse_and_compare full v1 v2 in
  let returns = returns_result ~printer function_to_test in
  returns,[
    ("0.0.0","0.0.0-foo",true,1);
    ("0.0.0","0.0.0-foo",true,1);
    ("0.0.1","0.0.0",true,1);
    ("1.0.0","0.9.9",true,1);
    ("0.10.0","0.9.0",true,1);
    ("0.99.0","0.10.0",true,1);
    ("2.0.0","1.2.3",true,1);
    ("v0.0.0","0.0.0-foo",false,1);
    ("v0.0.1","0.0.0",false,1);
    ("v1.0.0","0.9.9",false,1);
    ("v0.10.0","0.9.0",false,1);
    ("v0.99.0","0.10.0",false,1);
    ("v2.0.0","1.2.3",false,1);
    ("0.0.0","v0.0.0-foo",false,1);
    ("0.0.1","v0.0.0",false,1);
    ("1.0.0","v0.9.9",false,1);
    ("0.10.0","v0.9.0",false,1);
    ("0.99.0","v0.10.0",false,1);
    ("2.0.0","v1.2.3",false,1);
    ("1.2.3","1.2.3-asdf",true,1);
    ("1.2.3","1.2.3-4",true,1);
    ("1.2.3","1.2.3-4-foo",true,1);
    ("1.2.3-5-foo","1.2.3-5",true,1);
    ("1.2.3-5","1.2.3-4",true,1);
    ("1.2.3-5-foo","1.2.3-5-Foo",true,1);
    ("3.0.0","2.7.2+asdf",true,1);
    ("1.2.3-a.10","1.2.3-a.5",true,1);
    ("1.2.3-a.b","1.2.3-a.5",true,1);
    ("1.2.3-a.b","1.2.3-a",true,1);
    ("1.2.3-a.b.c.10.d.5","1.2.3-a.b.c.5.d.100",true,1);
    ("1.2.3-r2","1.2.3-r100",true,1);
    ("1.2.3-r100","1.2.3-R2",true,1)
  ]

let test_parse_and_compare_eq =
  let function_to_test (v1,v2,full) = parse_and_compare full v1 v2 in
  let returns = returns_result ~printer function_to_test in
  returns,[
    ("1.2.3","v1.2.3",true,0);
    ("1.2.3","=1.2.3",false,0);
    ("1.2.3","v 1.2.3",false,0);
    ("1.2.3","= 1.2.3",false,0);
    ("1.2.3"," v1.2.3",false,0);
    ("1.2.3"," =1.2.3",false,0);
    ("1.2.3"," v 1.2.3",false,0);
    ("1.2.3"," = 1.2.3",false,0);
    ("1.2.3-0","v1.2.3-0",false,0);
    ("1.2.3-0","=1.2.3-0",false,0);
    ("1.2.3-0","v 1.2.3-0",false,0);
    ("1.2.3-0","= 1.2.3-0",false,0);
    ("1.2.3-0"," v1.2.3-0",false,0);
    ("1.2.3-0"," =1.2.3-0",false,0);
    ("1.2.3-0"," v 1.2.3-0",false,0);
    ("1.2.3-0"," = 1.2.3-0",false,0);
    ("1.2.3-1","v1.2.3-1",false,0);
    ("1.2.3-1","=1.2.3-1",false,0);
    ("1.2.3-1","v 1.2.3-1",false,0);
    ("1.2.3-1","= 1.2.3-1",false,0);
    ("1.2.3-1"," v1.2.3-1",false,0);
    ("1.2.3-1"," =1.2.3-1",false,0);
    ("1.2.3-1"," v 1.2.3-1",false,0);
    ("1.2.3-1"," = 1.2.3-1",false,0);
    ("1.2.3-beta","v1.2.3-beta",false,0);
    ("1.2.3-beta","=1.2.3-beta",false,0);
    ("1.2.3-beta","v 1.2.3-beta",false,0);
    ("1.2.3-beta","= 1.2.3-beta",false,0);
    ("1.2.3-beta"," v1.2.3-beta",false,0);
    ("1.2.3-beta"," =1.2.3-beta",false,0);
    ("1.2.3-beta"," v 1.2.3-beta",false,0);
    ("1.2.3-beta"," = 1.2.3-beta",false,0);
    ("1.2.3-beta+build"," = 1.2.3-beta+otherbuild",false,0);
    ("1.2.3+build"," = 1.2.3+otherbuild",false,0);
    ("1.2.3-beta+build","1.2.3-beta+otherbuild",true,0);
    ("1.2.3+build","1.2.3+otherbuild",true,0);
    ("  v1.2.3+build","1.2.3+otherbuild",true,0)
  ]


let make_test_cases_compare (assert_function,triplets) =
  List.map ( fun (v1,v2,full,result) -> 
    (Printf.sprintf "%s - %s" v1 v2) >:: assert_function result (v1,v2,full)
  ) triplets


(* Tests for the range parser *)

open RangeDesugar

let tests_parsing =
  [
    (SOr ((SLt (SVersion "2")), (SAnd (SGt (SVersion "3"), SLt (SVersion "4")))), "<2||>3 <4\n");
    (SHyphen (SVersion "1", SVersion "2"), "1 - 2\n");
    (SHyphen (SVersion "1.2.3", SVersion "2"), "1.2.3 - 2\n");
    (SStar, "\n");
    (SVersion "1.x", "1.x\n");
    (SVersion "1.2.x", "1.2.x\n");
    (STilde (SVersion "1.2"), "~1.2\n");
    (STilde (SVersion "1.2.3"), "~1.2.3\n");
    (STilde (SVersion "1.2"), "~ 1.2\n");
    (STilde (SVersion "1.2"), "~      1.2\n");
    (SCaret (SVersion "1.2"), "^1.2\n");
    (SCaret (SVersion "1.2"), "^  1.2\n");
    (*Node testing*)
    (SHyphen ((SVersion "1.0.0"), SVersion "2.0.0") , "1.0.0 - 2.0.0\n");
    (SCaret (SVersion "1.2.3+build"), "^1.2.3+build\n");
    (SGte SStar, ">=*\n");
    (SHyphen (SVersion "1.2.3pre+asdf", SVersion "2.4.3-pre+asdf"), "1.2.3pre+asdf - 2.4.3-pre+asdf\n");
    (SStar, "\n");
  ]


let tests_try_parse_version =
  [
    ((NumberItem 2, NumberItem 3, NumberItem 4, [], []), "2.3.4");
    ((NumberItem 2, NumberItem 3, EmptyItem, [], []), "2.3");
    ((NumberItem 2, EmptyItem, EmptyItem, [], []), "2");
    ((XItem, EmptyItem, EmptyItem, [], []), "x");
    ((NumberItem 2, XItem, EmptyItem, [], []), "2.x");
    ((NumberItem 2, XItem, XItem, [], []), "2.x.x");
    ((NumberItem 1, NumberItem 2, XItem, [], []), "1.2.x");
    ((NumberItem 2, XItem, XItem, [], []), "2.*.x");
    ((NumberItem 2, XItem, XItem, [], []), "2.*.*");
    ((NumberItem 1, NumberItem 2, NumberItem 3, [StringIdentifier "alpha"; NumericIdentifier 7], []), "1.2.3-alpha.7");
  ]

let tests_desugar =
  let v1_str = "1.2.3" in
  let v2_str = "2.3.4" in
  let v3_str = "1.2.0" in
  let v4_str = "1.3.0" in
  let v5_str = "2.0.0" in
  let v6_str = "1.0.0" in
  let v7_str = "0.2.3" in
  let v8_str = "0.3.0" in
  let v9_str = "0.2.0" in
  let v10_str = "0.0.3" in
  let v11_str = "0.0.4" in
  let v12_str = "0.1.0" in
  let v_zero_str = "0.0.0" in
  let v_beta_str = "1.2.3-beta-2" in
  let v1 = try_parse_version v1_str in
  let v2 = try_parse_version v2_str in
  let v3 = try_parse_version v3_str in
  let v4 = try_parse_version v4_str in
  let v5 = try_parse_version v5_str in
  let v6 = try_parse_version v6_str in
  let v7 = try_parse_version v7_str in
  let v8 = try_parse_version v8_str in
  let v9 = try_parse_version v9_str in
  let v10 = try_parse_version v10_str in
  let v11 = try_parse_version v11_str in
  let v12 = try_parse_version v12_str in
  let v_zero = try_parse_version v_zero_str in
  let v_beta = try_parse_version v_beta_str in
  [
    (*Desugar hyphen ranges*)
    (And (Gte (Version v1), Lt (Version v2)), SHyphen (SVersion v1_str, SVersion v2_str));
    (And (Gte (Version v3), Lt (Version v2)), SHyphen (SVersion "1.2", SVersion v2_str));
    (And (Gte (Version v2), Lt (Version v4)), SHyphen (SVersion v2_str, SVersion "1.2"));
    (And (Gte (Version v2), Lt (Version v5)), SHyphen (SVersion v2_str, SVersion "1"));
    (Gte (Version v_zero), SStar);
    (*Desugar x-ranges*)
    (Gte (Version v_zero), SStar);
    (And (Gte (Version v6), Lt (Version v5)), SVersion "1.x");
    (And (Gte (Version v6), Lt (Version v5)), SVersion "1.x.x");
    (And (Gte (Version v3), Lt (Version v4)), SVersion "1.2.x");
    (And (Gte (Version v6), Lt (Version v5)), SVersion "1");
    (And (Gte (Version v3), Lt (Version v4)), SVersion "1.2");
    (*Desugar tilde *)
    (And (Gte (Version v1), Lt (Version v4)), STilde (SVersion "1.2.3"));
    (And (Gte (Version v3), Lt (Version v4)), STilde (SVersion "1.2"));
    (And (Gte (Version v6), Lt (Version v5)), STilde (SVersion "1"));
    (And (Gte (Version v7), Lt (Version v8)), STilde (SVersion "0.2.3"));
    (And (Gte (Version v9), Lt (Version v8)), STilde (SVersion "0.2"));
    (And (Gte (Version v_zero), Lt (Version v6)), STilde (SVersion "0"));
    (And (Gte (Version v_beta), Lt (Version v4)), STilde (SVersion v_beta_str));
    (*Desugar caret*)
    (And (Gte (Version v10), Lt (Version v11)), SCaret (SVersion "0.0.3"));
    (And (Gte (Version v7), Lt (Version v8)), SCaret (SVersion "0.2.3"));
    (And (Gte (Version v1), Lt (Version v5)), SCaret (SVersion "1.2.3"));
    (And (Gte (Version v3), Lt (Version v5)), SCaret (SVersion "1.2.x"));
    (And (Gte (Version v_zero), Lt (Version v12)), SCaret (SVersion "0.0.x"));
    (And (Gte (Version v_beta), Lt (Version v5)), SCaret (SVersion v_beta_str));
    (And (Gte (Version v6), Lt (Version v5)), SCaret (SVersion "1.x"));
    (And (Gte (Version v_zero), Lt (Version v6)), SCaret (SVersion "0.x"));
  ]

let make_test_parse_range =
  let id x = x in
  List.map (fun (result, v) -> 
   (Printf.sprintf "%s" v) >:: fun _ -> assert_equal (string_of_expr result) (string_of_expr (RangeNode.parse v)) ~printer: id
  ) tests_parsing


let make_test_desugar =
  List.map (fun (result, v) -> 
   (Printf.sprintf "%s" (string_of_expr v)) >:: fun _ -> assert_equal result (RangeDesugar.desugar v) ~printer: string_of_range ~cmp: equal_range
  ) tests_desugar


let make_test_try_parse_version =
  List.map (fun (result, v) -> 
   (Printf.sprintf "%s" v) >:: fun _ -> assert_equal result (RangeDesugar.try_parse_version v) ~printer: string_of_version_range
  ) tests_try_parse_version


let suite = 
  "suite" >::: [ 
    "test_parse_version_major" >::: make_test_cases_parse test_parse_version_major;
    "test_parse_version_minor" >::: make_test_cases_parse test_parse_version_minor;
    "test_parse_version_patch" >::: make_test_cases_parse test_parse_version_patch;
    "test_parse_and_compare_gt" >::: make_test_cases_compare test_parse_and_compare_gt;
    "test_parse_and_compare_eq" >::: make_test_cases_compare test_parse_and_compare_eq;
    "test parsing ranges" >::: make_test_parse_range;
    "test desugar" >::: make_test_desugar;
    "test try desugar" >::: make_test_try_parse_version;
  ]

let main () = OUnit.run_test_tt_main suite ;;
main ()
