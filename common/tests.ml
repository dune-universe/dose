
open OUnit
open Common

let parse_uri =
  "parse uri" >::: [
    "deb" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "deb://path/to/file" 
      in assert_equal true (protocol = "deb" && path = "path/to/file")
    ) ;
    "hdlist" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "hdlist://path/to/file" 
      in assert_equal true (protocol = "hdlist" && path = "path/to/file")
    ) ;
    "synth" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "synth://path/to/file" 
      in assert_equal true (protocol = "synth" && path = "path/to/file")
    ) ;

    "cudf" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "cudf:///path/to/file" 
      in assert_equal true (protocol = "cudf" && path = "/path/to/file")
    ) ;
    "sqlite" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "sqlite:///path/to/file" 
      in assert_equal true (protocol = "sqlite" && path = "/path/to/file")
    ) ;
    (* TODO : I know that the dbname and queryOpt are wrong *)
    "pgsql" >:: (fun _ ->
      let (protocol,(userOpt,passOpt,hostOpt,portOpt,path),queryOpt) =
        Uri.parseUri "pgsql://test:tester@localhost/dbname?v1=k1&v2=k2"
      in assert_equal true (
        protocol = "pgsql" && 
        userOpt = Some "test" &&
        passOpt = Some "tester" &&
        hostOpt = Some "localhost" &&
        path = "dbname" &&
        queryOpt = Some "v1=k1&v2=k2"
      )
    ) ;
  ]

let all = 
  "all tests" >::: [
    parse_uri ;
  ]

let main () =
  OUnit.run_test_tt_main all
;;

main ()
