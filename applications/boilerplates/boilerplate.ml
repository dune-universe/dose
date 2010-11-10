open ExtLib
open Common

(*************************************************************)
(* Options *)
module type Ot = sig
  val options :
    ?usage:string ->
    ?version:string ->
    ?suppress_usage:bool ->
    ?suppress_help:bool ->
    ?prog:string ->
    ?formatter:OptParse.Formatter.t -> unit -> OptParse.OptParser.t
end

let and_sep_re = Pcre.regexp "\\s*;\\s*"
let pkg_re = Pcre.regexp "\\(([0-9a-z][a-z0-9.+-]*)\\s*,\\s*([a-zA-Z0-9.+:~-]+)\\)"
let parse_pkg s =
  let parse_aux str =
    try
      let s = Pcre.exec ~rex:pkg_re str  in
      (Pcre.get_substring s 1, Pcre.get_substring s 2)
    with
      Not_found -> (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Pcre.split ~rex:and_sep_re s);;

let pkglist_option ?default ?(metavar = "PKGLST") () =
  OptParse.Opt.value_option metavar default
  parse_pkg (fun _ s -> Printf.sprintf "invalid package list '%s'" s)

module MakeOptions(O : Ot) = struct
  open OptParse ;;

  let verbose = StdOpt.incr_option ()
  let progress = StdOpt.store_true ()
  let timers = StdOpt.store_true ()
  let options = O.options ~version:VersionInfo.version () ;;

  open OptParser ;;
  add options ~short_name:'v' ~long_name:"verbose" ~help:"print additional information" verbose;
  add options ~long_name:"progress" ~help:"print progress bars" progress;
  add options ~long_name:"timers" ~help:"print timing information" progress;

end

let enable_debug = function
  |0 -> () (* quite : default *)
  |1 ->
      Util.Info.all_enabled ()
  |2 ->
      Util.Info.all_enabled () ;
      Util.Warning.all_enabled ()
  |_ ->
      Util.Info.all_enabled () ;
      Util.Warning.all_enabled () ;
      Util.Debug.all_enabled ()
;;

let enable_bars verbose l =
  if verbose then List.iter Util.Progress.enable l
(* let enable_time = List.iter Util.Timer. *)

let debug fmt = Util.make_debug "Boilerplate" fmt
let info fmt = Util.make_info "Boilerplate" fmt
let warning fmt = Util.make_warning "Boilerplate" fmt

(*************************************************************)

let argv_ f l =
  let a = Array.of_list l in
  if Array.length a = 0 then
    (Printf.eprintf "No input file specified" ; exit 1)
  else f a

let argv1 l = argv_ (fun a -> a.(0)) l
let argv2 l = argv_ (fun a -> (a.(0),a.(1))) l
let argv3 l = argv_ (fun a -> (a.(0),a.(1),a.(2))) l

(** read a debian Packages file - compressed or not *)
let read_deb ?(extras=[]) s =
  let ch = Input.open_file s in
  let l = Debian.Packages.parse_packages_in ~extras (fun x->x) ch in
  let _ = (* IO.close_in ch *) Input.close_ch ch in
  l

(** transform a list of debian control stanza into a cudf packages list *)
let deb_load_list ?(extras=[]) ?(status=[]) l =
  let l = Debian.Packages.merge status l in
  let tables = Debian.Debcudf.init_tables l in
  let pkglist = List.map (Debian.Debcudf.tocudf ~extras tables) l in
  let from_cudf (p,i) = (p,Debian.Debcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  (pkglist,from_cudf,to_cudf)

let pp_versions_table fmt (from_cudf, pkglist) =
  List.iter (fun pkg ->
    let (p,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in
    Format.fprintf fmt "%s=%d=%s@." p pkg.Cudf.version v
  ) pkglist

(** transform a list of debian control stanza into a cudf packages list *)
let eclipse_load_list ?(extras=[]) ?(status=[]) l =
  let tables = Eclipse.Eclipsecudf.init_tables l in
  let pkglist = List.map (Eclipse.Eclipsecudf.tocudf ~extras tables) l in
  let from_cudf (p,i) = (p,Eclipse.Eclipsecudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p,Eclipse.Eclipsecudf.get_cudf_version tables (p,v)) in
  (pkglist,from_cudf,to_cudf)

(** transform a list of debian control stanza into a cudf universe *)
let deb_load_universe ?(extras=[]) l =
  let (l,f,t) = deb_load_list ~extras l in
  (Cudf.load_universe l, f, t)

(* XXX double minded ... this code is kinda similar to the code in rpmcudf 
 * refactor or not refactor ? *)
(** transform a list of rpm control stanza into a cudf packages list *)
let rpm_load_list l =
IFDEF HASRPM THEN
  let tables =  Rpm.Rpmcudf.init_tables l in
  let pkglist = List.map (Rpm.Rpmcudf.tocudf tables) l in
  Rpm.Rpmcudf.clear tables;
  let from_cudf (p,i) = (p,string_of_int i) in
  let to_cudf (p,v) = failwith "Nope ..." in
  (pkglist,from_cudf,to_cudf)
ELSE
  failwith "librpm not available. re-configure with --with-rpm"
END

(** transform a list of rpm control stanza into a cudf universe *)
let rpm_load_universe l =
  let (l,f,t) = rpm_load_list l in
  (Cudf.load_universe l, f, t)

(** parse a cudf file and return a triple (preamble,package list,request
    option). If the package is not valid fails and exit *)
let parse_cudf doc =
  try
    let p = Cudf_parser.from_IO_in_channel (*open_in doc*) (Input.open_file doc) in
    Cudf_parser.parse p
  with
  |Cudf_parser.Parse_error _
  | Cudf.Constraint_violation _ as exn -> begin
    Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
    doc (Printexc.to_string exn);
    exit (-1)
  end

let load_cudf_ch ch =
  try
    let p = Cudf_parser.from_IO_in_channel ch in
    Cudf_parser.load p
  with
  |Cudf_parser.Parse_error _
  | Cudf.Constraint_violation _ as exn -> begin
    Printf.eprintf "Error while loading CUDF: %s\n%!"
    (Printexc.to_string exn);
    exit (-1)
  end

(** parse a cudf file and return a triple (preamble,universe,request option).
    If the package is not valid fails and exit *)
let load_cudf doc = 
  let ch = Input.open_file doc in
  let l = load_cudf_ch ch in
  Input.close_ch ch;
  l

(* XXX when parsing a cudf, I should also remember the preamble !! *)
let cudf_load_list file =
  let _, pkglist, _ = parse_cudf file in
  let from_cudf (p,i) = (p,string_of_int i) in
  let to_cudf (p,v) = (p,int_of_string v) in
  (pkglist,from_cudf,to_cudf)

let cudf_load_universe file =
  let (l,f,t) = cudf_load_list file in
  (Cudf.load_universe l, f, t)

(* return a list of file of the same type *)
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

(** return the name of the file *)
let unpack (_,(_,_,_,_,file),_) = file

(** parse a list of uris of the same type and return a cudf packages list *)
let parse_input ?(default_arch=None) ?(extras=[]) uris =
  match filter None [] uris with
  |("cudf",[("cudf",(_,_,_,_,file),_)]) ->
      cudf_load_list file
  |("debstdin", [p]) ->
      let l = Debian.Packages.input_raw_ch ~default_arch (IO.input_channel stdin) in
      deb_load_list ~extras l
  |("deb", l) ->
      let filelist = List.map unpack l in
      let l = Debian.Packages.input_raw ~default_arch filelist in
      deb_load_list ~extras l
  |("eclipse", l) ->
      let filelist = List.map unpack l in
      let l = Eclipse.Packages.input_raw filelist in
      eclipse_load_list ~extras l
  |("pgsql"|"sqlite"), [(("pgsql"|"sqlite") as dbtype,info,(Some query))] ->
IFDEF HASDB THEN
        let db = Db.Backend.init_database dbtype info (Idbr.parse_query query) in
        let l = Db.Backend.load_selection db (`All) in
        deb_load_list ~extras l
ELSE
      failwith (dbtype^" Not supported. re-configure with --with-??")
END
  |("hdlist", l) -> 
IFDEF HASRPM THEN
      let filelist = List.map unpack l in
      let l = Rpm.Packages.Hdlists.input_raw filelist in
      rpm_load_list l
ELSE
    failwith ("hdlist Not supported. re-configure with --with-rpm")
END
  |("synth", l) -> 
IFDEF HASRPM THEN
      let filelist = List.map unpack l in
      let l = Rpm.Packages.Synthesis.input_raw filelist in
      rpm_load_list l
ELSE
    failwith ("synth Not supported. re-configure with --with-rpm")
END
    |(s,_) -> failwith (s^" Not supported")
;;

(** parse and merge a list of files into a cudf package list *)
let load_list ?(default_arch=None) ?(extras=[]) uris =
  info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let u = parse_input ~default_arch ~extras uris in
  Util.Timer.stop timer u
;;

(** parse and merge a list of files into a cudf universe *)
let load_universe ?(default_arch=None) ?(extras=[]) uris =
  info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let (l,f,t) = parse_input ~default_arch ~extras uris in
  let u = (Cudf.load_universe l, f, t) in
  Util.Timer.stop timer u
;;

(*
(* XXX to refactor in Borilerplate.ml *)
let parse uri =
  Printf.eprintf "Parsing and normalizing...%!" ;
  let timer = Common.Util.Timer.create "Parsing and normalizing" in
  Common.Util.Timer.start timer;
  let pkglist =
    match Input.parse_uri uri with
    |("deb",(_,_,_,_,file),_) -> begin
      let l = Debian.Packages.input_raw [file] in
      let tables = Debian.Debcudf.init_tables l in
      List.map (Debian.Debcudf.tocudf tables) l
    end
    |("cudf",(_,_,_,_,file),_) -> begin
      let _, l, _ = Boilerplate.parse_cudf file in l
    end
    |_ -> failwith "Not supported"
  in
  ignore(Common.Util.Timer.stop timer ());
  Printf.eprintf "done\n%!" ;
  pkglist
;;
*)


