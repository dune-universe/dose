(* Tests for the range parser *)

open RangeDesugar
open Versioning.SemverNode
open OUnit

let tests_parsing =
  [
    (SOr ((SLt (SVersion "2")), (SAnd (SGt (SVersion "3"), SLt (SVersion "4")))), "<2||>3 <4\n");
    (SHyphen (SVersion "1", SVersion "2"), "1 - 2\n");
    (SHyphen (SVersion "1.2.3", SVersion "2"), "1.2.3 - 2\n");
    (SEq SStar, "\n");
    (SEq (SVersion "1.x"), "1.x\n");
    (SEq (SVersion "1.2.x"), "1.2.x\n");
    (SEq (SVersion "1.2.3"), "1.2.3\n");
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


(**Parser of dependencies of npm*)

let tests_parse_deps =
  [
    ([], "{}");
    ([("hello", "world")], "{\"hello\":\"world\"}");
    ([("hello", "world"); ("xxx", "yyy")], "{\"hello\":\"world\",\"xxx\":\"yyy\"}");
    ([("hello", "1.2.3-beta.4"); ("xxx", "yyy")], "{\"hello\":\"1.2.3-beta.4\",\"xxx\":\"yyy\"}");
  ]

let make_test_parse_deps =
  List.map (fun (result, x) ->
    (x >:: fun _ -> assert_equal result (RangeNode.parse_npm_deps x))
  ) tests_parse_deps


(*Tests for parsing dependencies of npm*)

open Packages

let tests_parse_depends =
  [
    ([], "{}");
    ([[(("p1", None), Some ("=", "1.2.3"))]], "{\"p1\":\"1.2.3\"}");
    ([[(("p1", None), Some (">=", "1.2.3")); (("p1", None), Some ("<<", "1.3.0"))]], "{\"p1\":\"~1.2.3\"}");
  ]


let make_test_parse_depends =
  let function_to_test par =
    let f = Pef.Packages.parse_s ~default:"" ~required:true Pef.Packages.parse_string in
    (parse_deps (f "depends" par))
  in
  List.map (fun (result, x) ->
    let argument = [("depends", (Common.Format822.dummy_loc, x))] in
    x >:: fun _ -> assert_equal result (function_to_test argument)
  ) tests_parse_depends

let suite = 
  "suite" >::: [ 
    "test parsing ranges" >::: make_test_parse_range;
    "test desugar" >::: make_test_desugar;
    "test try desugar" >::: make_test_try_parse_version;
    "test parse deps" >::: make_test_parse_deps;
    "test parse depends" >::: make_test_parse_depends;
  ]

let main () = OUnit.run_test_tt_main suite ;;
main ()
