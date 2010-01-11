open ExtLib
open Common

let enable_debug ?(bars=[]) () =
  List.iter Util.Progress.enable bars;
  Util.set_verbosity Common.Util.Summary
;;

let argv_ f l =
  let a = Array.of_list l in
  if Array.length a = 0 then
    (Printf.eprintf "No input file specified" ; exit 1)
  else f a

let argv1 l = argv_ (fun a -> a.(0)) l
let argv2 l = argv_ (fun a -> (a.(0),a.(1))) l
let argv3 l = argv_ (fun a -> (a.(0),a.(1),a.(2))) l

let deb_load_universe l =
  let tables = Debian.Debcudf.init_tables l in
  let univ = Cudf.load_universe (List.map (Debian.Debcudf.tocudf tables) l) in
  let from_cudf pkg =
    let (p,i) = (pkg.Cudf.package,pkg.Cudf.version) in
    let v = Debian.Debcudf.get_real_version tables (p,i) in
    (p,v)
  in
  let to_cudf (p,v) =
    let i = Debian.Debcudf.get_cudf_version tables (p,v) in
    (p,i)
  in
  (univ,from_cudf,to_cudf)

let rpm_load_universe l =
  let univ = Rpm.Rpmcudf.load_universe l in
  let from_cudf pkg = (pkg.Cudf.package,string_of_int pkg.Cudf.version) in
  let to_cudf (p,v) = failwith "Nope ..." in
  (univ,from_cudf,to_cudf)

let cudf_load_universe file =
  let _, univ, _ = CudfAdd.load_cudf file in
  let from_cudf pkg = (pkg.Cudf.package,string_of_int pkg.Cudf.version) in
  let to_cudf (p,v) = failwith "Nope ..." in
  (univ,from_cudf,to_cudf)

let load_universe uri =
  Util.print_info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let u =
    match Input.parse_uri uri with
    |(("pgsql"|"sqlite") as dbtype,info,(Some query)) ->
IFDEF HASDB THEN
        let db = Db.Backend.init_database dbtype info (Idbr.parse_query query) in
        let l = Db.Backend.load_selection db (`All) in
        deb_load_universe l
ELSE
      failwith (dbtype^" Not supported")
END
    |("deb",(_,_,_,_,"-"),_) ->
      let l = Debian.Packages.input_raw_ch (IO.input_channel stdin) in
      deb_load_universe l
    |("deb",(_,_,_,_,file),_) ->
      let l = Debian.Packages.input_raw [file] in
      deb_load_universe l
    |("cudf",(_,_,_,_,file),_) ->
      cudf_load_universe file
    |("hdlist",(_,_,_,_,file),_) ->
IFDEF HASRPM THEN
      let l = Rpm.Packages.Hdlists.input_raw [file] in
      rpm_load_universe l
ELSE
    failwith ("hdlist Not supported")
END
    |("synth",(_,_,_,_,file),_) ->
IFDEF HASRPM THEN
      let l = Rpm.Packages.Synthesis.input_raw [file] in
      rpm_load_universe l
ELSE
    failwith ("synth Not supported")
END
    |(s,_,_) -> failwith (s^" Not supported")
  in
  Util.Timer.stop timer u
;;

