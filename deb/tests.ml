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

let extras_properties = [
  ("Maintainer", ("maintainer", `String None));
  ("Size", ("size", `Nat None));
  ("Installed-Size", ("installedsize", `Nat None))
];;

let extras = List.map fst extras_properties ;;
let options = { Debcudf.default_options with Debcudf.extras_opt = extras_properties } ;;

let packagelist = Packages.input_raw [f_packages] ;;
let tables = Debcudf.init_tables packagelist ;;
let cudf_list = List.map (Debcudf.tocudf tables ~options) packagelist ;; 
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
  (* let clusters = Debutil.cluster packagelist in *)
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

let test_multiarch = 
  "test multiarch" >::: [
    "multi arch same provide-conflicts" >:: (fun _ -> 
      (*
      let f = Filename.concat test_dir "deb/edsp/multiarch-same-provides.edsp" in
      let (request,pkglist) = Edsp.input_raw ~archs:["arch1";"arch2"] f in
      let tables = Debcudf.init_tables pkglist in
      let options = {
        Debcudf.default_options with
        Debcudf.native = "arch1";
        Debcudf.foreign = ["arch2"] }
      in
      let default_preamble =
        let l = List.map snd Edsp.extras_tocudf in
        CudfAdd.add_properties Debcudf.preamble l
      in
      let cudf_pkglist = List.map (fun pkg -> Edsp.tocudf tables ~options pkg) pkglist in
      let universe = Cudf.load_universe cudf_pkglist in
      let cudf_request = Edsp.requesttocudf tables universe request in
      let r = Algo.Depsolver.check_request (Some default_preamble,cudf_pkglist,cudf_request) in
      assert_equal (Algo.Diagnostic.is_solution r) true
      *) 
      ()
    );
  ] 
;;

let test_numbering = 
  "test numbering" >::: [
    "sequence" >:: (fun _ -> 
      try
        let debconf = Cudf.lookup_package universe ("debconf",32) in
        assert_equal debconf.Cudf.version 32
      with Not_found -> assert_failure "debconf version mismatch"
    );
    "get real version" >:: (fun _ -> ());
  ] 
;;

let test_virtual = 
  "test virtual" >::: [
    "provides" >:: (fun _ -> 
      try
        let v = Debcudf.get_cudf_version tables ("ssmtp","2.62-3") in
        let ssmtp = Cudf.lookup_package universe ("ssmtp",v) in
        let vpkg = ("--virtual-mail-transport-agent",None) in
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
        let v = Debcudf.get_cudf_version tables ("ssmtp","2.62-3") in
        let ssmtp = Cudf.lookup_package universe ("ssmtp",v) in
        assert_equal true (List.mem (ssmtp.Cudf.package,None) ssmtp.Cudf.conflicts)
      with Not_found -> assert_failure "ssmtp version mismatch"
    );
  ]

let test_mapping =
  "test deb -> cudf mapping" >::: [
(*    test_numbering ; *)
    test_virtual;
    test_multiarch
  ]

(* Parsing tests *)

(* Useful test functions *)

let returns_result function_to_test expected_result =
  (fun args () -> assert_equal (function_to_test args) expected_result)
and raises_failure function_to_test failure_text =
  (fun args () -> assert_raises (Failure failure_text) (fun () -> function_to_test args) )

(* let ch = Input.open_file f_packages ;; *)
(* Extension of "bracket_tmpfile" function, filling
    the temporary file with lines from the given list. *)
let bracket_tmpfile_filled lines (test_fun : string -> unit)  =
  bracket_tmpfile
    (fun (file, ch) ->
      List.iter (fun line ->
        output_string ch (line ^ "\n")
      ) lines;
      close_out ch;
      test_fun file
    )
;;

(* parse_inst *)

(* List of triplets: (test_name, file_lines, expected_result) *)
let parse_inst_triplets =
  (* The standard expected result. *)
  let result =
  [ (("name1", "version1"), ());
    (("name2", "version2"), ());
    (("name3", "version3"), ());
    (("name4", "version4"), ()) ]
  in
  (* List of triplets. *)
  [ ("simple",
     [ "ii name1 version1";
       "ii name2 version2";
       "ii name3 version3";
       "ii name4 version4" ],
     result);
    ("varied blanks and comments",
     [ "ii name1 version1 blah blah blah";
       "ii 	 name2 	       version2		 blah blah blah";
       "ii   name3    version3  blah blah blah";
       "ii name4                         version4 blah blah blah" ],
     result);
    ("errors with something else than \"ii\" at the beginning",
     [ "ii name1 version1 blah blah blah";
       "jj errname1 errversion1 blah blah blah";
       "ii name2 version2 blah blah blah";
       "ii name3 version3 blah blah blah";
       "kk errname2 errversion2 blah blah blah";
       "ii name4 version4 blah blah blah" ],
     result);
    ("errors with no \"ii\" at all",
     [ "err1";
       "err2 err3";
       "";
       "err4 err5 err6";
       "ii name1 version1";
       "err7 err8";
       "ii name2 version2";
       "";
       "ii name3 version3";
       "ii name4 version4" ],
     result);
    ("varying number of fields",
     [ "";
       "ii";
       "ii errname1";
       "ii name1 version1";
       "ii name2 version2 blah";
       "ii name3 version3 blah blah";
       "ii name4 version4 blah blah blah" ],
     result)
  ]

let list_of_hashtbl ht = 
  ExtLib.List.of_enum (ExtLib.Hashtbl.enum ht)

let test_parse_inst = 
  let parse_inst_test_cases =
    List.map (fun (test_name, file_lines, expected_result) ->
      test_name >::
      bracket_tmpfile_filled file_lines
	( fun file -> assert_equal
	    (list_of_hashtbl (Apt.parse_inst_from_file file))
	    expected_result )
	) parse_inst_triplets
  in
  "test parse_inst" >::: parse_inst_test_cases


(* parse_popcon *)
let parse_popcon_triplets =
  let function_to_test = Apt.parse_popcon in
  let returns = returns_result function_to_test
  and raises  = raises_failure function_to_test
  in
  [ ("simple",      "123 name1 456",      returns (123, "name1", 456) );
    ("more fields", "123 name1 456 err1", returns (123, "name1", 456) );
    ("wrong int 1", "err1 name1 456",     raises "int_of_string" );
    ("wrong int 2", "123 name1 err2",     raises "int_of_string" )
  ] 

(* parse_pkg_req *)
let parse_pkg_req_triplets =
  let function_to_test = (fun (suite, s) -> Apt.parse_pkg_req suite s) in
  let returns = returns_result function_to_test
(*  and raises  = raises_failure function_to_test *)
  in
  [ ("suite name=1.2", 
     ( (Some "suite"), "name=1.2"), 
     returns (None, (("name", None), Some ("=", "1.2")), Some "suite"));

    ("suite +name=1.2", 
     ( (Some "suite"), "+name=1.2"), 
     returns (Some Format822.I, (("name", None), Some ("=", "1.2")), Some "suite"));

    ("suite -name=1.2", 
     ( (Some "suite"), "-name=1.2"), 
     returns (Some Format822.R, (("name", None), Some ("=", "1.2")), Some "suite"));

    ("suite name/suite1", 
     ( (Some "suite"), "name/suite1"), 
     returns (None, (("name", None), None), Some "suite"));

    ("none name/suite1", 
     ( None, "name/suite1"), 
     returns (None, (("name", None), None), Some "suite1"));

    ("none name", 
     ( None, "name"), 
     returns (None, (("name", None), None), None));

    ("none +name", 
     ( None, "+name"), 
     returns (Some Format822.I, (("name", None), None), None));

    ("none -name", 
     ( None, "-name"), 
     returns (Some Format822.R, (("name", None), None), None));

    ("suite name", 
     ( Some "suite", "name"), 
     returns (None, (("name", None), None), Some "suite"));

  ]

(* parse_pref_labels *)
let parse_pref_labels_triplets =
  let function_to_test = Apt.parse_pref_labels in
  let returns = returns_result function_to_test
(*  and raises  = raises_failure function_to_test *)
  in
  [ ("simple single num",      "123",         returns [("v", "123")]);
    ("simple single alpha",    "abc",         returns [("a", "abc")]);
    ("simple pair",            "123=abc",     returns [("123", "abc")]);
    ("complicated single num", "123.456.789", returns [("v", "123.456.789")]);
    ("many nums",              "123,456,789", returns [("v", "123"); ("v", "456"); ("v", "789")]);
    ("many alphas",            "abc,def,ghi", returns [("a", "abc"); ("a", "def"); ("a", "ghi")]);
    ("many pairs",             "1=a,2=b,3=c", returns [("1", "a"); ("2", "b"); ("3", "c")]);
  ]

(* parse_pref_package *)
let parse_pref_package_triplets =
  let function_to_test = (fun s -> Apt.parse_pref_package ((),s)) in
  let returns = returns_result function_to_test
(*  and raises  = raises_failure function_to_test *)
  in
  [ ("asterisk 1", "*",          returns Apt.Pref.Star);
    ("asterisk 2", "    *     ", returns Apt.Pref.Star);
    ("name 1",     "name1",      returns (Apt.Pref.Package (Packages.parse_name (Format822.dummy_loc, "name1")))); ]

(* parse_pin *)
let parse_pin_triplets =
  let function_to_test = (fun s -> Apt.parse_pin ((),s)) in
  let returns = returns_result function_to_test
(*  and raises  = raises_failure function_to_test *)
  in
  [ ("release 1", "release name1", returns (Apt.Pref.Release (Apt.parse_pref_labels "name1")));
    ("version 1", "version name1", returns (Apt.Pref.Version "name1"));
    ("origin 1",  "origin name1",  returns (Apt.Pref.Origin "name1")); ]

(* Makes a list of test cases from a list of triplets:
    (test_name, string_to_parse, assert_function) *)
let make_test_cases triplets =
  List.map ( fun (test_name, input, assert_function) -> test_name >:: assert_function input ) triplets

let test_parsing =
  "test_parsing" >::: [
    test_parse_inst;
    "test parse_popcon"       >::: make_test_cases parse_popcon_triplets;
    "test parse_pkg_req"      >::: make_test_cases parse_pkg_req_triplets;
    "test parse_pref_labels"  >::: make_test_cases parse_pref_labels_triplets;
    "test parse_pref_package" >::: make_test_cases parse_pref_package_triplets;
    "test_parse_pin"          >::: make_test_cases parse_pin_triplets;
  ]

let select_deps =
  let function_to_test = (fun (archs,profile,dep) -> Sources.select archs profile dep) in
  let returns = returns_result function_to_test in
  (* testname archlist profilename     pkg    archlist           profilelist           return *)
  [ ("00", ("amd64", None,          ("foo", [],                [])),                 returns (Some "foo"));
    ("01", ("amd64", None,          ("foo", [],                [(true,"stage1")])),  returns None);
    ("02", ("amd64", None,          ("foo", [],                [(false,"stage1")])), returns (Some "foo"));
    ("03", ("amd64", None,          ("foo", [(true,"amd64")],  [])),                 returns (Some "foo"));
    ("04", ("amd64", None,          ("foo", [(true,"amd64")],  [(true,"stage1")])),  returns None);
    ("05", ("amd64", None,          ("foo", [(true,"amd64")],  [(false,"stage1")])), returns (Some "foo"));
    ("06", ("amd64", None,          ("foo", [(false,"amd64")], [])),                 returns None);
    ("07", ("amd64", None,          ("foo", [(false,"amd64")], [(true,"stage1")])),  returns None);
    ("08", ("amd64", None,          ("foo", [(false,"amd64")], [(false,"stage1")])), returns None);
    ("09", ("amd64", Some "stage1", ("foo", [],                [])),                 returns (Some "foo"));
    ("10", ("amd64", Some "stage1", ("foo", [],                [(true,"stage1")])),  returns (Some "foo"));
    ("11", ("amd64", Some "stage1", ("foo", [],                [(false,"stage1")])), returns None);
    ("12", ("amd64", Some "stage1", ("foo", [(true,"amd64")],  [])),                 returns (Some "foo"));
    ("13", ("amd64", Some "stage1", ("foo", [(true,"amd64")],  [(true,"stage1")])),  returns (Some "foo"));
    ("14", ("amd64", Some "stage1", ("foo", [(true,"amd64")],  [(false,"stage1")])), returns None);
    ("15", ("amd64", Some "stage1", ("foo", [(false,"amd64")], [])),                 returns None);
    ("16", ("amd64", Some "stage1", ("foo", [(false,"amd64")], [(true,"stage1")])),  returns None);
    ("17", ("amd64", Some "stage1", ("foo", [(false,"amd64")], [(false,"stage1")])), returns None);
    ("18", ("i386",  None,          ("foo", [],                [])),                 returns (Some "foo"));
    ("19", ("i386",  None,          ("foo", [],                [(true,"stage1")])),  returns None);
    ("20", ("i386",  None,          ("foo", [],                [(false,"stage1")])), returns (Some "foo"));
    ("21", ("i386",  None,          ("foo", [(true,"amd64")],  [])),                 returns None);
    ("22", ("i386",  None,          ("foo", [(true,"amd64")],  [(true,"stage1")])),  returns None);
    ("23", ("i386",  None,          ("foo", [(true,"amd64")],  [(false,"stage1")])), returns None);
    ("24", ("i386",  None,          ("foo", [(false,"amd64")], [])),                 returns (Some "foo"));
    ("25", ("i386",  None,          ("foo", [(false,"amd64")], [(true,"stage1")])),  returns None);
    ("26", ("i386",  None,          ("foo", [(false,"amd64")], [(false,"stage1")])), returns (Some "foo"));
    ("27", ("i386",  Some "stage1", ("foo", [],                [])),                 returns (Some "foo"));
    ("28", ("i386",  Some "stage1", ("foo", [],                [(true,"stage1")])),  returns (Some "foo"));
    ("29", ("i386",  Some "stage1", ("foo", [],                [(false,"stage1")])), returns None);
    ("30", ("i386",  Some "stage1", ("foo", [(true,"amd64")],  [])),                 returns None);
    ("31", ("i386",  Some "stage1", ("foo", [(true,"amd64")],  [(true,"stage1")])),  returns None);
    ("32", ("i386",  Some "stage1", ("foo", [(true,"amd64")],  [(false,"stage1")])), returns None);
    ("33", ("i386",  Some "stage1", ("foo", [(false,"amd64")], [])),                 returns (Some "foo"));
    ("34", ("i386",  Some "stage1", ("foo", [(false,"amd64")], [(true,"stage1")])),  returns (Some "foo"));
    ("35", ("i386",  Some "stage1", ("foo", [(false,"amd64")], [(false,"stage1")])), returns None);
  ]

let test_sources_input = "
Package: source1
Version: 0.1-1
Architecture: any
Build-Depends: bin1, bin2:any, bin3:native

Package: source2
Version: 0.1-1
Architecture: any
Build-Depends: bin1 [amd64] <!stage1>, bin2 | bin3 <stage1>, bin4 [!amd64] <!stage1>

Package: source3
Version: 0.1-1
Architecture: any
Build-Depends: bin1, bin2
Build-Depends-Indep: bin3
"
;;

let test_sources2packages =
  let data = IO.input_string test_sources_input in
  let packagelist = Sources.parse_sources_in "" data in
  let builddeparch = "amd64" in
  let sources = Sources.sources2packages ~profiles:true builddeparch packagelist in
  let function_to_test src =
    let src = List.find (fun s -> s.Packages.name = src) sources in
    src.Packages.depends
  in
  let returns = returns_result function_to_test in
  [
    (
      "any/native", "src:source1", returns [
        [(("build-essential", Some "native"), None)];
        [(("bin1", None), None)];
        [(("bin2", Some "any"), None)];
        [(("bin3", Some "native"), None)]
      ]
    );
    (
      "default", "src:source2", returns [
        [(("build-essential", Some "native"), None)];
        [(("bin1", None), None)];
        [(("bin2", None), None)]
      ]
    );
    (
      "stage1", "src-stage1:source2", returns [
        [(("build-essential", Some "native"), None)];
        [
          (("bin2", None), None);
          (("bin3", None), None)
        ]
      ]
    );
    (
      "indep", "src:source3", returns [
        [(("build-essential", Some "native"), None)];
        [(("bin3", Some "native"), None)];
        [(("bin1",None), None)];
        [(("bin2",None), None)]
      ]
    )
  ]
;;

let test_sources =
  "test_sources" >::: [
    "test select" >::: make_test_cases select_deps;
    "test sources2packages" >::: make_test_cases test_sources2packages;
  ]
;;

let all = 
  "all tests" >::: [ 
    test_parsing;
    test_mapping ;
    test_conflicts;
    test_version;
    test_cluster;
    test_evolution;
    test_version_comparison;
    test_architecture_matching;
    test_sources
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
