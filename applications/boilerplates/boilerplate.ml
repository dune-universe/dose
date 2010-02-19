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

let deb_load_list ?(extras=[]) l =
  let tables = Debian.Debcudf.init_tables l in
  let pkglist = List.map (Debian.Debcudf.tocudf ~extras tables) l in
  let from_cudf pkg =
    let (p,i) = (pkg.Cudf.package,pkg.Cudf.version) in
    let v = Debian.Debcudf.get_real_version tables (p,i) in
    (p,v)
  in
  let to_cudf (p,v) =
    let i = Debian.Debcudf.get_cudf_version tables (p,v) in
    (p,i)
  in
  (pkglist,from_cudf,to_cudf)

let deb_load_universe ?(extras=[]) l =
  let (l,f,t) = deb_load_list ~extras l in
  (Cudf.load_universe l, f, t)

(* XXX double minded ... this code is kinda similar to the code in rpmcudf 
 * refactor or not refactor ? *)
let rpm_load_list l =
IFDEF HASRPM THEN
  let tables =  Rpm.Rpmcudf.init_tables l in
  let pkglist = List.map (Rpm.Rpmcudf.tocudf tables) l in
  Rpm.Rpmcudf.clear tables;
  let from_cudf pkg = (pkg.Cudf.package,string_of_int pkg.Cudf.version) in
  let to_cudf (p,v) = failwith "Nope ..." in
  (pkglist,from_cudf,to_cudf)
ELSE
  failwith "Rpm input not available"
END

let rpm_load_universe l =
  let (l,f,t) = rpm_load_list l in
  (Cudf.load_universe l, f, t)

let cudf_load_list file =
  let _, pkglist, _ = CudfAdd.parse_cudf file in
  let from_cudf pkg = (pkg.Cudf.package,string_of_int pkg.Cudf.version) in
  let to_cudf (p,v) = failwith "Nope ..." in
  (pkglist,from_cudf,to_cudf)

let cudf_load_universe file =
  let (l,f,t) = cudf_load_list file in
  (Cudf.load_universe l, f, t)

let rec filter init acc uris =
  match uris,init with
  |[],None -> (Printf.eprintf "No input provided\n"; exit 1)
  |[],Some init -> (init,acc)
  |uri::tail, _ ->
    begin match Input.parse_uri uri, init with
    |("cudf",(_,_,_,_,"-"),_) as p, None when tail = [] -> ("cudfstdin",[p])
    |("cudf",(_,_,_,_,"-"),_), _ when tail <> [] -> (Printf.eprintf "Only one cudf stdin input allowed\n"; exit 1)

    |("cudf",_,_) as p, None when tail = [] -> ("cudf",[p])
    |("cudf",_,_), _ when tail <> [] -> (Printf.eprintf "Only one cudf input allowed\n"; exit 1)

    |("deb",(_,_,_,_,"-"),_) as p, None when tail = [] -> ("debstdin",[p])
    |("deb",(_,_,_,_,"-"),_), _ when tail <> [] -> (Printf.eprintf "Only one deb stdin input allowed\n"; exit 1)

    |(("pgsql"|"sqlite") as dbtype,_,_) as p, None when tail = [] -> (dbtype,[p])
    |(("pgsql"|"sqlite"),_,_), None when tail <> [] -> (Printf.eprintf "Only one db input allowed\n"; exit 1)

    |(t,_,_) as p, None -> filter (Some t) (p::acc) tail
    |(t,_,_) as p, Some i when t = i -> filter (Some t) (p::acc) tail

    |(t,_,_),_ -> (Printf.eprintf "You cannot mix different input types\n"; exit 1)
    end

let parse_input ?(extras=[]) uris =
  let filelist typ = function
    |(t,(_,_,_,_,file),_) when t = typ -> file
    |_ -> assert false
  in
  match filter None [] uris with
  |("cudf",[("cudf",(_,_,_,_,file),_)]) ->
      cudf_load_list file
  |("debstdin", [p]) ->
      let l = Debian.Packages.input_raw_ch (IO.input_channel stdin) in
      deb_load_list ~extras l
  |("deb", l) ->
      let filelist = List.map (filelist "deb") l in
      let l = Debian.Packages.input_raw filelist in
      deb_load_list ~extras l
  |("pgsql"|"sqlite"), [(("pgsql"|"sqlite") as dbtype,info,(Some query))] ->
IFDEF HASDB THEN
        let db = Db.Backend.init_database dbtype info (Idbr.parse_query query) in
        let l = Db.Backend.load_selection db (`All) in
        deb_load_list ~extras l
ELSE
      failwith (dbtype^" Not supported")
END
  |("hdlist", l) -> 
IFDEF HASRPM THEN
      let filelist = List.map (filelist "hdlist") l in
      let l = Rpm.Packages.Hdlists.input_raw filelist in
      rpm_load_list l
ELSE
    failwith ("hdlist Not supported")
END
  |("synth", l) -> 
IFDEF HASRPM THEN
      let filelist = List.map (filelist "synth") l in
      let l = Rpm.Packages.Synthesis.input_raw filelist in
      rpm_load_list l
ELSE
    failwith ("synth Not supported")
END
    |(s,_) -> failwith (s^" Not supported")
;;

(* parse and merge a list of files into a cudf package list *)
let load_list ?(extras=[]) uris =
  Util.print_info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let u = parse_input ~extras uris in
  Util.Timer.stop timer u
;;

(* parse and merge a list of files into a cudf universe *)
let load_universe ?(extras=[]) uris =
  Util.print_info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let (l,f,t) = parse_input ~extras uris in
  let u = (Cudf.load_universe l, f, t) in
  Util.Timer.stop timer u
;;

