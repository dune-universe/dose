
open IprLib
open OUnit

let ipr_pkg1 = { Ipr.default_package with Ipr.name = "/bin/bash" ; version = "1" }
let ipr_pkg2 = { Ipr.default_package with Ipr.name = "bla*(&^*&" ; version = "1" }
let ipr_pkg3 = { Ipr.default_package with Ipr.name = "b" ; version = "1" }
let ipr_pkg4 = { Ipr.default_package with Ipr.name = "ba" ; version = "1.2" }
let ipr_pkg5 = { Ipr.default_package with Ipr.name = "ba" ; version = "1.3" ; conflicts = ["ba",Some("=","1.1")] }

let cudf_pkg1 = { Cudf.default_package with Cudf.package = "%2fbin%2fbash" ; version = 1 }
let cudf_pkg2 = { Cudf.default_package with Cudf.package = "bla%2a%28%26%5e%2a%26" ; version = 1 }
let cudf_pkg3 = { Cudf.default_package with Cudf.package = "%2f%2fb" ; version = 1 }
let cudf_pkg4 = { Cudf.default_package with Cudf.package = "ba" ; version = 2 }
let cudf_pkg5 = { Cudf.default_package with Cudf.package = "ba" ; version = 3 }

let ipr_pkg_list = [ ipr_pkg1 ; ipr_pkg2 ; ipr_pkg3 ; ipr_pkg4 ; ipr_pkg5 ]
let cudf_pkg_list = [ cudf_pkg1 ; cudf_pkg2 ; cudf_pkg3 ; cudf_pkg4 ; cudf_pkg5 ]

let _ = Ipr.init_tables ipr_pkg_list ;;

(*
Printf.eprintf "%s\n" (Cudf_printer.string_of_package (Ipr.tocudf false ipr_pkg1));;
Printf.eprintf "%s\n" (Cudf_printer.string_of_package (Ipr.tocudf false ipr_pkg2));;
Printf.eprintf "%s\n" (Cudf_printer.string_of_package (Ipr.tocudf false ipr_pkg3));;
Printf.eprintf "%s\n" (Cudf_printer.string_of_package (Ipr.tocudf false ipr_pkg4));;
Printf.eprintf "%s\n" (Cudf_printer.string_of_package (Ipr.tocudf false ipr_pkg5));;
*)

let test_escaping =
  "test escaping" >::: [
    "starts with" >:: (fun _ -> 
      assert_equal true (Cudf.(=%) (Ipr.tocudf ipr_pkg1) cudf_pkg1)
    );

    "special chars" >:: (fun _ -> 
      assert_equal true (Cudf.(=%) (Ipr.tocudf ipr_pkg2) cudf_pkg2)
    );

    "single char" >:: (fun _ -> 
      assert_equal true (Cudf.(=%) (Ipr.tocudf ipr_pkg3) cudf_pkg3)
    );
  ]

let test_numbering = 
  "test numbering" >::: [
    "sequence" >:: (fun _ -> 
      let p1 = Ipr.tocudf ipr_pkg4 in
      let p2 = Ipr.tocudf ipr_pkg5 in
      assert_equal true (
        (p1.Cudf.version = 2) &&
        (p2.Cudf.version = 3)
      )
    ) ;
  ]

let all = 
  "all tests" >::: [ 
    test_escaping ;
    test_numbering
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
