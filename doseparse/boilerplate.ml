(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common

let debug fmt = Util.make_debug "Boilerplate" fmt
let info fmt = Util.make_info "Boilerplate" fmt
let warning fmt = Util.make_warning "Boilerplate" fmt
let fatal fmt = Util.make_fatal "Boilerplate" fmt

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

(* *************************************** *)

let and_sep_re = Pcre.regexp "\\s*,\\s*"
let pkg_re = Pcre.regexp "([0-9a-z][a-z0-9.+-]*)(\\s*$|\\s*\\(([><=!]+)\\s+([a-zA-Z0-9.+:~-]+)\\))"
let parse_vpkg s =
  let parse_aux str =
    try
      let s = Pcre.exec ~rex:pkg_re str  in
      let p = Pcre.get_substring s 1 in
      try 
        let c = Pcre.get_substring s 3 in
        let v = Pcre.get_substring s 4 in
        (p,CudfAdd.cudfop(Some(c,v)))
      with Not_found -> (p,None)
    with
      Not_found -> fatal "Parse error %s\n" str
  in List.map parse_aux (Pcre.split ~rex:and_sep_re s)
;;

let vpkglist_option ?default ?(metavar = "VPKGLST") () =
  OptParse.Opt.value_option metavar default
  parse_vpkg (fun _ s -> Printf.sprintf "invalid vpackage list '%s'" s)
;;

(* *************************************** *)

let and_sep_re = Pcre.regexp "\\s*;\\s*"
let pkg_re = Pcre.regexp "\\(([0-9a-z][a-z0-9.+-]*)\\s*,\\s*([a-zA-Z0-9.+:~-]+)\\)"
let parse_pkg s =
  let parse_aux str =
    try
      let s = Pcre.exec ~rex:pkg_re str  in
      (Pcre.get_substring s 1, Pcre.get_substring s 2)
    with
      Not_found -> fatal "Parse error %s" str
  in List.map parse_aux (Pcre.split ~rex:and_sep_re s)
;;

let pkglist_option ?default ?(metavar = "PKGLST") () =
  OptParse.Opt.value_option metavar default
  parse_pkg (fun _ s -> Printf.sprintf "invalid package list '%s'" s)
;;

(* *************************************** *)

module MakeOptions(O : Ot) = struct
  open OptParse ;;

  let verbose = StdOpt.incr_option ()
  let progress = StdOpt.store_true ()
  let timers = StdOpt.store_true ()
  let options = O.options ~version:VersionInfo.version () ;;

  open OptParser ;;
  add options ~short_name:'v' ~long_name:"verbose" ~help:"print additional information" verbose;
  add options ~long_name:"progress" ~help:"print progress bars" progress;
  add options ~long_name:"timers" ~help:"print timing information" timers;

end

let enable_debug = function
  |0 -> () (* quite : default *)
  |1 -> Util.Info.all_enabled ()
  |2 ->
      begin
        Util.Info.all_enabled () ;
        Util.Warning.all_enabled ()
      end
  |_ ->
      begin
        Util.Info.all_enabled () ;
        Util.Warning.all_enabled () ;
        Util.Debug.all_enabled ()
      end
;;

let enable_bars verbose l =
  if verbose then List.iter Util.Progress.enable l

let enable_timers verbose l = 
  at_exit (Util.Timer.dump Format.err_formatter);
  if verbose then List.iter Util.Timer.enable l
;;

(*************************************************************)

let argv_ f l =
  let a = Array.of_list l in
  if Array.length a = 0 then
    fatal "No input file specified"
  else f a

let argv1 l = argv_ (fun a -> a.(0)) l
let argv2 l = argv_ (fun a -> (a.(0),a.(1))) l
let argv3 l = argv_ (fun a -> (a.(0),a.(1),a.(2))) l

(** read a debian Packages file - compressed or not *)
let read_deb ?filter ?(extras=[]) fname =
  let ch = Input.open_file fname in
  let l = Debian.Packages.input_raw_ch ?filter ~extras ch in
  let _ = Input.close_ch ch in
  l

(** transform a list of debian control stanza into a cudf packages list *)
let deb_load_list ?(extras=[]) ?(status=[]) l =
  let l = if status = [] then l else Debian.Packages.merge status l in
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
  (* Rpm.Rpmcudf.clear tables; *)
  let from_cudf (p,i) = (p,string_of_int i) in
  let to_cudf (p,v) = (p,Rpm.Rpmcudf.get_cudf_version tables (p,v)) in
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
    let p = Cudf_parser.from_IO_in_channel (Input.open_file doc) in
    Cudf_parser.parse p
  with
  |Cudf_parser.Parse_error _
  | Cudf.Constraint_violation _ as exn -> begin
    fatal "Error while loading CUDF from %s: %s" doc (Printexc.to_string exn)
  end

(** parse a cudf file and return a triple (preamble,universe,request option).
    If the package is not valid fails and exit *)
let load_cudf doc = 
  let ch = Input.open_file doc in
  let l = 
    try
      let p = Cudf_parser.from_IO_in_channel ch in
      Cudf_parser.load p
    with
    |Cudf_parser.Parse_error _
    |Cudf.Constraint_violation _ as exn -> begin
      fatal "Error while loading CUDF file %s:\n%s" doc (Printexc.to_string exn)
    end 
  in
  Input.close_ch ch;
  l
;;

(* XXX when parsing a cudf, I should also remember the preamble !! *)
let cudf_load_list file =
  let _, pkglist, _ = parse_cudf file in
  let from_cudf (p,i) = (p,string_of_int i) in
  let to_cudf (p,v) = (p,int_of_string v) in
  (pkglist,from_cudf,to_cudf)

let cudf_load_universe file =
  let (l,f,t) = cudf_load_list file in
  (Cudf.load_universe l, f, t)

(* Check that all uris are of type that is an instance of scheme *)
(* If yes return that instance of scheme, and the list of pathes *)
(* in uris.                                                      *)
let rec filter opt_scheme acc uris =
  match uris,opt_scheme with
  |[],None -> fatal "No input provided"
  |[],Some real_scheme -> (real_scheme,acc)
  |uri::tail, _ ->
    begin match Input.parse_uri uri, opt_scheme with
    (* XXX add support !
    |(Url.Cudf,(_,_,_,_,"-"),_) as p, None when tail = [] -> ("cudfstdin",[p])
    |(Url.Cudf,(_,_,_,_,"-"),_), _ when tail <> [] -> fatal "Only one cudf stdin input allowed"
    *)

    |(Url.Cudf,_,_) as p, None when tail = [] -> (Url.Cudf,[p])
    |(Url.Cudf,_,_), _ when tail <> [] -> fatal "Only one cudf input allowed"

    |(Url.Deb,(_,_,_,_,"-"),_), None when tail = [] && acc = []-> (Url.Deb,[]) (* stdin *)
    |(Url.Deb,(_,_,_,_,"-"),_), _ when tail <> [] || acc <> [] -> fatal "Only one deb stdin input allowed"

    |((Url.Pgsql|Url.Sqlite) as dbtype,_,_) as p, None when tail = [] && acc = [] -> (dbtype,[p])
    |((Url.Pgsql|Url.Sqlite),_,_), None when tail <> [] || acc <> [] -> fatal "Only one db input allowed"

    |(t,_,_) as p, None -> filter (Some t) (p::acc) tail
    |(t,_,_) as p, Some i when t = i -> filter (Some t) (p::acc) tail

    |(t,_,_),_ -> fatal "You cannot mix different input types";
    end

(** return the name of the file *)
let unpack (_,(_,_,_,_,file),_) = file

(** parse a list of uris of the same type and return a cudf packages list *)
let parse_input ?default_arch ?(extras=[]) uris =
  match filter None [] uris with
  |(Url.Cudf,[(Url.Cudf,(_,_,_,_,file),_)]) ->
      cudf_load_list file
  |(Url.Deb, []) ->
      let l = Debian.Packages.input_raw_ch ?default_arch (IO.input_channel stdin) in
      deb_load_list ~extras l
  |(Url.Deb, l) ->
      let filelist = List.map unpack l in
      let l = Debian.Packages.input_raw ?default_arch filelist in
      deb_load_list ~extras l
  |(Url.Eclipse, l) ->
      let filelist = List.map unpack l in
      let l = Eclipse.Packages.input_raw filelist in
      eclipse_load_list ~extras l
  |(Url.Pgsql|Url.Sqlite), [((Url.Pgsql|Url.Sqlite) as dbtype,info,(Some query))] ->
IFDEF HASDB THEN
        let db = Db.Backend.init_database dbtype info (Idbr.parse_query query) in
        let l = Db.Backend.load_selection db (`All) in
        deb_load_list ~extras l
ELSE
    fatal "%s Not supported. re-configure with --with-??" (Url.scheme_to_string dbtype)
END
  |(Url.Hdlist, l) -> 
IFDEF HASRPM THEN
      let filelist = List.map unpack l in
      let l = Rpm.Packages.Hdlists.input_raw filelist in
      rpm_load_list l
ELSE
    fatal "hdlist Not supported. re-configure with --with-rpm"
END
  |(Url.Synthesis, l) -> 
IFDEF HASRPM THEN
      let filelist = List.map unpack l in
      let l = Rpm.Packages.Synthesis.input_raw filelist in
      rpm_load_list l
ELSE
    fatal "synthesis input format not supported. re-configure with --with-rpm"
END
    |(s,_) -> fatal "%s Not supported" (Url.scheme_to_string s)
;;

let supported_formats () =
  let standard = ["cudf://";"deb://";"deb://-";"eclipse://"] in
  let rpm = 
IFDEF HASRPM THEN
     ["hdlist://";"synthesis://"]
ELSE
     []
END
   in
   let db =
IFDEF HASDB THEN
     ["pgsql://";"sqlite://"]
ELSE
     []
END
   in
   standard@rpm@db
;;

(** parse and merge a list of files into a cudf package list *)
let load_list ?default_arch ?(extras=[]) uris =
  info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let u = parse_input ?default_arch ~extras uris in
  Util.Timer.stop timer u
;;

(** parse and merge a list of files into a cudf universe *)
let load_universe ?default_arch ?(extras=[]) uris =
  info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let (l,f,t) = parse_input ?default_arch ~extras uris in
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


