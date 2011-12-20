(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2011 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                            *)
(*  Contributions 2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>             *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)



open OUnit
open Debian
open Common

let test_dir = "tests"

let f_packages = Filename.concat test_dir "DebianPackages/Packages.bz2" ;;
let f_release = Filename.concat test_dir "DebianPackages/Release" ;;
let f_discriminants = Filename.concat test_dir "deb/discriminants" ;;

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

(* version comparison ****************************************************************************)

let version_test_cases = [
  ("1.2-5.6","3.4-5.8",-1);      (* easy *)
  ("1:2-3","2:2-3",-1);          (* period comparison - equal *)
  ("1:2-3","1:2-3",0);           (* period comparison - less *)
  ("2:2-3","1:2-3",1);           (* period comparison - greater *)
  ("0:1.2-3","1.2-3",0);         (* period =0 when missing *)
  ("000001:2-3","2:1",-1);       (* leading 0 in period *)
  ("00:1","0000:1",0);           (* leading 0 in period *)
  ("1",":1",0);                  (* epoch separator but no epoch *)
  ("1a","1c",-1);                (* character ordering *)
  ("1z","1A",1);                 (* character ordering *)
  ("1Z","1.",-1);                (* character ordering *)
  ("1.","1-",1);                 (* character ordering *)
  ("1-","1+",-1);                (* character ordering *)
  ("1~~","1~~a",-1);             (* tilde - example from policy *)
  ("1~~a","1~",-1);              (* tilde - example from policy *)
  ("1~","1",-1);                 (* tilde - example from policy *)
  ("1","1a",-1);                 (* tilde - example from policy *)
  ("000","00",0);                (* numerical comparison - zeros *)
  ("1a000","1a",0);              (* empty string in numerical part counts as 0 *)
  ("1-000","1",0);               (* empty string in numerical part counts as 0 *)
  ("1.23","1.23~",1);            (* tilde after numerical part *)
  ("1.2+a.3","1.2+a.3",0);       (* alternating lexical and numerical *)
  ("1.2+a.3","1.2+aa.3",1);      (* alternating lexical and numerical *)
  ("1.2+a.3","1.2+a~.3",1);      (* alternating lexical and numerical *)
  ("05","000001",1);             (* skiping leading zeros *)
  ("1a","1a00000~",1);
  ("2:1","3:1",-1);              (* hierarchy of parts *)
  ("2:1.1.1","2:1.1.2",-1);      (* hierarchy of parts *)
  ("2:1.1-1.1","2:1.1-1.2",-1);  (* hierarchy of parts *)
];;

let dpkg_compare x y =
  let c1 = Printf.sprintf "dpkg --compare-versions %s lt %s" x y in
  let c2 = Printf.sprintf "dpkg --compare-versions %s eq %s" x y in
  if (Sys.command c1) = 0 then -1
  else if (Sys.command c2) = 0 then 0
  else 1
;;

let test_version_comparison = 
  "debian version comparison" >::: [
    "" >:: (fun _ ->
      (* we might want to execute these tests also on a non debian machine *)
      let debian_machine = ref true in
      List.iter (fun (v1,v2,res) ->
        let dose_cmp = Version.compare v1 v2 in
        let dpkg_cmp = if !debian_machine then dpkg_compare v1 v2 else res in
        if dose_cmp <> dpkg_cmp then begin
          Printf.eprintf "error version comparison %s %s\n" v1 v2;
          Printf.eprintf "dpkg says %d\n" dpkg_cmp;
          Printf.eprintf "dose says %d\n" dose_cmp
        end;
        assert_equal dose_cmp dpkg_cmp
      ) version_test_cases
    )
  ]
;;

(* architecture matching *****************************************************************)

let architecture_test_cases = [
  ("all", "i386", true);               (* all matches everything *)
  ("any", "kfreebsd-amd64",true);      (* any matches everything *)
  ("amd64", "i386", false);            (* pattern and arch do not split *)
  ("toaster", "toaster", true);        
  ("hurd-amd64", "hurd-amd64", true);  (* pattern and arch split *)
  ("hurd-amd64", "netbsd-amd64", false);   
  ("hurd-amd64", "hurd-i386", false);
  ("hurd-amd64", "netbsd-i386", false);
  ("hurd-amd64", "amd64", false);      (* pattern splits, arch doesn't *)
  ("hurd-amd64", "i386", false);
  ("linux-amd64", "amd64", true);
  ("linux-amd64", "i386", false);
  ("amd64", "hurd-amd64", false);      (* arch splits,patten doesn't *)
  ("amd64", "hurd-i386", false);
  ("amd64", "linux-amd64", true);
  ("amd64", "linux-i386", false);
  ("any-amd64", "hurd-amd64", true);   (* OS pattern *)
  ("any-amd64", "linux-amd64", true);
  ("any-amd64", "hurd-i386", false);
  ("any-amd64", "linux-i386", false);
  ("hurd-any", "hurd-alpha", true);    (* CPU pattern *)
  ("linux-any", "linux-alpha", true);
  ("hurd-any", "netbsd-alpha", false);
  ("linux-any", "netbsd-alpha", false);
  ("any-any", "linux-i386", true);     (* OS and CPU pattern *)
  ("any-any", "hurd-i386", true);
  ("any-any", "amd64", true)
];;

let test_architecture_matching =
  "debian architecture matching" >::: [
    "" >:: (fun _ ->
      List.iter
	(fun (source,arch,expected) ->
	  let result = Architecture.src_matches_arch source arch  in
	  if result <> expected
	  then
	    begin
	      Printf.printf "error matching architecture %s against %s\n" source arch;
	      Printf.printf "found %b, should be %b\n" result expected
	    end;
	  assert_equal result expected
	)
	architecture_test_cases
    )
  ]
;;

(*****************************************************************************************)

let test_version = 
  "debian version parsing" >::: [
    "splitting all" >:: (fun _ ->
      let v = "1:1.4-5+b1" in
      let (e,u,r,b) = Version.split v in
      assert_equal (e,u,r,b) ("1","1.4","5","b1")
    );
    "normalize all" >:: (fun _ ->
      let v = "1:1.4-5+b1" in
      assert_equal (Version.normalize v) "1.4-5"
    );
    "concat all" >:: (fun _ ->
      let v = "1:1.4-5+b1" in
      assert_equal (Version.concat (Version.split v)) v
    );
    "splitting partial 1" >:: (fun _ ->
      let v = "1.4-5+b1" in
      let (e,u,r,b) = Version.split v in
      assert_equal (e,u,r,b) ("","1.4","5","b1")
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
    "splitting partial 4" >:: (fun _ ->
      let v = "1.1+b6" in
      let (e,u,r,b) = Version.split v in
      assert_equal (e,u,r,b) ("","1.1","","b6")
    );
    "normalize partial 4" >:: (fun _ ->
      let v = "1.1+b6" in
      assert_equal (Version.normalize v) "1.1"
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
  let packagelist = Packages.input_raw [f_discriminants] in
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
        List.iter (fun (version,realversion,cluster) ->
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
  let packagelist = Packages.input_raw [f_discriminants] in
  let constraints_table = Evolution.constraints packagelist in
  let clusters = Debutil.cluster packagelist in
  "evolution" >::: [
    "constraints" >:: (fun _ ->
      let constr = Evolution.all_constraints constraints_table "cc" in
      (* List.iter (fun (c,v) -> Printf.printf "(%s %s)\n" (string_of_relop c) v ) constr;
       * *)
      assert_equal [(`Eq,"4");(`Lt,"3")] constr
    );
    "constraints empty" >:: (fun _ ->
      let constr = Evolution.all_constraints constraints_table "hh" in
      (*
      List.iter (fun (c,v) -> Printf.printf "(%s %s)\n" (string_of_relop c) v ) constr;
      *)
      assert_equal [] constr
    );
    "versions" >:: (fun _ ->
      let vl = Evolution.all_versions [(`Gt,"3"); (`Eq,"3"); (`Lt,"4")] in
      (* List.iter (Printf.printf "-<< %s <<") vl; *)
      assert_equal ["4";"3"] vl
    );
    "range (1)" >:: (fun _ ->
      let rl = Evolution.range ["3.4";"76"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Eq "3.4");(`In ("3.4","76"));(`Eq "76");(`Hi "76")] rl
    );
    "range (2)" >:: (fun _ ->
      let rl = Evolution.range ["1"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Eq "1");(`Hi "1")] rl
    );
    "range bottom (1)" >:: (fun _ ->
      let rl = Evolution.range ~bottom:true ["3";"4"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Lo "3");(`Eq "3");(`In ("3","4"));(`Eq "4");(`Hi "4")] rl
    );
    "range bottom (2)" >:: (fun _ ->
      let rl = Evolution.range ~bottom:true ["1"] in
      (* List.iter (fun r -> Printf.printf "%s\n" (Evolution.string_of_range r)) rl; *)
      assert_equal [(`Lo "1");(`Eq "1");(`Hi "1")] rl
    );
    (*
    "evalsel" >:: (fun _ ->
      assert_equal false (Evolution.evalsel Version.compare ((`Eq "3.4"),(`Gt,"76")));
      assert_equal false (Evolution.evalsel Version.compare ((`In ("3.4","76")),(`Gt,"76")));
      assert_equal false (Evolution.evalsel Version.compare ((`Eq "76"),(`Gt,"76")));
      (*
      assert_equal true (Evolution.evalsel Version.compare ((`Hi "76"),(`Gt,"76")));
      *)
    );
    "discriminants simple" >:: (fun _ ->
      let assert_delay = assert_delay_stub [ ] in
      let constr = [(`Gt,"76")] in
      let vl = ["3.4";"76"] in
      let discr = Evolution.discriminant evalsel vl constr in
      List.iter (fun (target,equiv) -> 
        Printf.eprintf "(3) %s\n%!" (Evolution.string_of_range target);
        List.iter (fun k ->
          Printf.eprintf "(3) e %s\n%!" (Evolution.string_of_range k);
        ) equiv;
      ) discr;
      List.iter (fun (target,equiv) -> assert_delay (target,equiv)) discr 
    );
    "discriminant (single)" >:: (fun _ ->
      let assert_delay = assert_delay_stub [ (`Lo "1",[`Hi "1"]); (`Eq "1",[]) ] in
      let constr = Evolution.all_constraints constraints_table "bb" in
      let vl = Evolution.all_versions constr in
      let discr = Evolution.discriminant ~bottom:true vl constr in
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
          ("bb","1","1",[(`Eq "1",[]);(`Hi "1",[])]);
          ("aa","1","1",[]);
          ("ee_source","2","2",[]);
          ("ee_source","1","1",[]);
          ("cc_source","1","2",[
            (`Eq "4",[]);
            (`In ("2","3"),[]);
            (`Eq "2",[`Hi "4";`In ("3","4");`Eq "3"]);
            ]);
          ("cc_source","1","1",[
            (`In ("1","3"),[]);
            (`Eq "1",[`Hi "3";`Eq "3"])
            ]);
        ]
      in
      Hashtbl.iter (fun (sourcename, sourceversion) l ->
        Printf.eprintf "(2)cluster (%s,%s)\n%!" sourcename sourceversion; 
        List.iter (fun (version,cluster) ->
          let filter x =
            match Debian.Version.split version, Debian.Version.split x with
            |(_,v,_,_),(_,w,_,_) -> (Debian.Version.compare v w) <= 0
          in
          let l = Evolution.discriminants ~filter constraints_table cluster in
          Printf.eprintf "(2)v : %s\n%!" version;
          List.iter (fun (target,equiv) ->
            Printf.eprintf "(2)d : %s\n%!" (Evolution.string_of_range target);
            List.iter (fun target ->
            Printf.eprintf "(2)d : e %s\n%!" (Evolution.string_of_range target);
            ) equiv;
            Printf.eprintf "(2)d : ----\n%!"
          ) l;
          assert_delay (sourcename,sourceversion,version,l);
        ) l
      ) clusters;
      assert_equal true true
    );
    *)
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
        assert_equal debconf.Cudf.version 32
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
        let provides = CudfAdd.who_provides universe vpkg in
        assert_equal true (List.exists (Cudf.(=%) ssmtp) provides)
      with Not_found -> assert_failure "ssmtp version mismatch"
    );
    "virtual real" >:: (fun _ -> ())
  ]

let test_conflicts =
  "test conflict" >::: [
    "self conflict" >:: (fun _ -> 
      try 
        let ssmtp = Cudf.lookup_package universe ("ssmtp",8366) in
        assert_equal true (List.mem (ssmtp.Cudf.package,None) ssmtp.Cudf.conflicts)
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
    test_evolution;
    test_version_comparison;
    test_architecture_matching
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
