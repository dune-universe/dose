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

(* XXX TODO:
  * add test for default arch
  * add test for extras
  * add test for parsing errors
  * add test for Ingore Packages
  * add test for status and merge
  * *)

let ch = Input.open_file f_packages ;;
let extras_properties = [
  ("Maintainer", ("maintainer", `String None));
  ("Size", ("size", `Nat None));
  ("Installed-Size", ("installedsize", `Nat None))
];;
let extras = List.map fst extras_properties ;;
let packagelist = Packages.input_raw [f_packages] ;;
let tables = Debcudf.init_tables packagelist ;;
let cudf_list = List.map (Debcudf.tocudf ~extras:extras_properties tables) packagelist ;; 
let universe = Cudf.load_universe cudf_list ;;
let maps = CudfAdd.build_maps universe ;;

let test_version = 
  "debian version parsing" >::: [
    "splitting all" >:: (fun _ ->
      let v = "1:1.4-5+b1" in
      let (e,u,r,b) = Version.split v in
      assert_equal (e,u,r,b) ("1","1.4","5","+b1")
    );
    "normalize all" >:: (fun _ ->
      let v = "1:1.4-5+b1" in
      assert_equal (Version.normalize v) "1.4-5"
    );
    "splitting partial 1" >:: (fun _ ->
      let v = "1.4-5+b1" in
      let (e,u,r,b) = Version.split v in
      assert_equal (e,u,r,b) ("","1.4","5","+b1")
    );
    "normalize partial 1" >:: (fun _ ->
      let v = "1.4-5+b1" in
      assert_equal (Version.normalize v) "1.4-5"
    );
    "splitting partial 2" >:: (fun _ ->
      let v = "1.4" in
      let (e,u,r,b) = Version.split v in
      assert_equal (e,u,r,b) ("","1.4","","")
    );
    "normalize partial 2" >:: (fun _ ->
      let v = "1.4" in
      assert_equal (Version.normalize v) "1.4"
    );
    "splitting partial 3" >:: (fun _ ->
      let v = "0" in
      let (e,u,r,b) = Version.split v in
      assert_equal (e,u,r,b) ("","0","","")
    );
    "normalize partial 3" >:: (fun _ ->
      let v = "0" in
      assert_equal (Version.normalize v) "0"
    );
  ]
;;

let string_of_relop = function
  |`Eq -> "="
  |`Neq -> "!="
  |`Geq -> ">="
  |`Gt -> ">"
  |`Leq -> "<="
  |`Lt -> "<"
;;

let rec assert_delay_stub l =
  let acc = ref l in
  fun e ->
    match !acc with
    |[] -> assert_failure "OUnit: not equal"
    |h::tl -> begin
        acc := tl;
        assert_equal e h
    end
;;

let test_cluster =
  let packagelist = Packages.input_raw ["tests/discriminants"] in
  let clusters = Debutil.cluster packagelist in
  "cluster" >::: [
    "groups" >:: (fun _ -> 
      let assert_delay = 
        assert_delay_stub [
          ("bb","1",[("bb","1")]);
          ("aa","1",[("aa","1")]);
          ("ee_source","2",[("ff","2")]);
          ("ee_source","1",[("gg","1");("ee","1")]);
          ("cc_source","1",[("cc","2")]);
          ("cc_source","1",[("dd","1")]);
        ]
      in

      Hashtbl.iter (fun (sourcename, sourceversion) l ->
        (* Printf.eprintf "(1)cluster (%s,%s)\n%!" sourcename sourceversion; *)
        List.iter (fun (version,cluster) ->
          (* Printf.eprintf "(1)v %s\n%!" version; *)
          let l = List.map(fun pkg -> (pkg.Packages.name,pkg.Packages.version)) cluster in
          (*
          List.iter (fun pkg ->
            Printf.eprintf "(1)pkg %s %s\n%!" pkg.Packages.name pkg.Packages.version
          ) cluster;
          *)
          assert_delay (sourcename,sourceversion,l);
        ) l
      ) clusters
    ); 
  ]
;;


let test_evolution =
  let packagelist = Packages.input_raw ["tests/discriminants"] in
  let constraints_table = Evolution.constraints packagelist in
  let clusters = Debutil.cluster packagelist in
  "evolution" >::: [
    "constraints" >:: (fun _ ->
      let constr = Evolution.all_constraints constraints_table "cc" in
      (* List.iter (fun (c,v) -> Printf.printf "(%s %s)\n" (string_of_relop c) v ) constr;
       * *)
      assert_equal [(`Eq,"4");(`Lt,"3")] constr
    );
    "versions" >:: (fun _ ->
      let vl = Evolution.all_versions [(`Gt,"3"); (`Eq,"3"); (`Lt,"4")] in
      (* List.iter (Printf.printf "-<< %s <<") vl; *)
      assert_equal ["4";"3"] vl
    );
    "range (1)" >:: (fun _ ->
      let rl = Evolution.range ~downgrade:true ["3";"4"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Lo "3");(`Eq "3");(`In ("3","4"));(`Eq "4");(`Hi "4")] rl
    );
    "range (2)" >:: (fun _ ->
      let rl = Evolution.range ~downgrade:true ["1"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Lo "1");(`Eq "1");(`Hi "1")] rl
    );
    "discriminant (single)" >:: (fun _ ->
      let assert_delay = assert_delay_stub [ (`Lo "1",[`Hi "1"]); (`Eq "1",[]) ] in
      let constr = Evolution.all_constraints constraints_table "bb" in
      let vl = Evolution.all_versions constr in
      let discr = Evolution.discriminant ~downgrade:true vl constr in
      (*
      List.iter (fun (target,equiv) -> 
        Printf.eprintf "(3) %s\n%!" (Evolution.string_of_range target);
        List.iter (fun k ->
          Printf.eprintf "(3) e %s\n%!" (Evolution.string_of_range k);
        ) equiv;
      ) discr;
      *)
      List.iter (fun (target,equiv) -> assert_delay (target,equiv)) discr 
    ); 
    "discriminant (cluster)" >:: (fun _ ->
      let assert_delay = 
        assert_delay_stub [
          ("bb","1","1",[(`Hi "1",[]);(`Eq "1",[])]);
          ("aa","1","1",[]);
          ("ee_source","2","2",[]);
          ("ee_source","1","1",[]);
          ("cc_source","1","2",[(`Eq "4",[]);(`Eq "3",[`Hi "4";`In ("3","4")])]);
          ("cc_source","1","1",[(`Eq "3",[`Hi "3"])]);
        ]
      in
      Hashtbl.iter (fun (sourcename, sourceversion) l ->
        (*
        Printf.eprintf "(2)cluster (%s,%s)\n%!" sourcename sourceversion; 
        *)
        List.iter (fun (version,cluster) ->
          let filter x =
            match Debian.Version.split version, Debian.Version.split x with
            |(_,v,_,_),(_,w,_,_) -> (Debian.Version.compare v w) <= 0
          in
          let l = Evolution.discriminants ~filter constraints_table cluster in
          (*
          Printf.eprintf "(2)v : %s\n%!" version;
          List.iter (fun (target,equiv) ->
            Printf.eprintf "(2)d : %s\n%!" (Evolution.string_of_range target);
            List.iter (fun target ->
            Printf.eprintf "(2)d : e %s\n%!" (Evolution.string_of_range target);
            ) equiv;
            Printf.eprintf "(2)d : ----\n%!"
          ) l;
          *)
          assert_delay (sourcename,sourceversion,version,l);
        ) l
      ) clusters;
      assert_equal true true
    );
    "align (with epoch)" >:: (fun _ ->
      let r = Evolution.align "1:3.4+b5" (`In ("3.5","3.6")) in
      assert_equal r (`In ("1:3.5","1:3.6"))
    );
    "align (without epoch 1)" >:: (fun _ ->
      let r = Evolution.align "3.4+b5" (`In ("2:3.5","2:3.6")) in
      assert_equal r (`In ("2:3.5","2:3.6"))
    );
    "align (without epoch 2)" >:: (fun _ ->
      let r = Evolution.align "3.4+b5" (`In ("3.5","3.6")) in
      assert_equal r (`In ("3.5","3.6"))
    );
(*
    "migration" >:: (fun _ ->
      Hashtbl.iter (fun (sourcename, sourceversion) h ->
        Hashtbl.iter (fun version cluster ->
          let migrationlist = Debian.Evolution.migrate cluster (`Lo "1:3") in
          List.iter (fun ((pkg,target),newtarget) -> 
            Printf.eprintf "%s %s\n%!" pkg.Packages.name pkg.Packages.version;
            Printf.eprintf "old %s\n%!" (Evolution.string_of_range target);
            Printf.eprintf "new %s\n%!" (Evolution.string_of_range newtarget)
          ) migrationlist
        ) h
      ) clusters
    );
*)
  ]
;;

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
;;

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
(*    test_numbering ; *)
    test_virtual
  ]

let all = 
  "all tests" >::: [ 
    test_mapping ;
    test_conflicts;
    test_version;
    test_cluster;
    test_evolution
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
