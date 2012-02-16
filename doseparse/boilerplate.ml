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

let debug fmt = Util.make_debug __FILE__ fmt
let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt
let fatal fmt = Util.make_fatal __FILE__ fmt

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

(* this is a ,-separated list of vpkgs of the form "a (= v)" *)
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

(* this is a ;-separated list of package names *)
let pkglist_option ?default ?(metavar = "PKGLST") () =
  OptParse.Opt.value_option metavar default
  parse_pkg (fun _ s -> Printf.sprintf "invalid package list '%s'" s)
;;

let incr_str_list ?(default=Some []) ?(metavar = "STR") =
  let acc = ref [] in 
  let coerce s = acc := s :: !acc ; !acc in
  fun () ->
  OptParse.Opt.value_option metavar default coerce 
  (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* this is a ,-separated list of strings *)
let str_list_option ?(default=Some []) ?(metavar = "STRLST") =
  let sep = "," in
  let coerce s = ExtString.String.nsplit s sep in
  fun () ->
    OptParse.Opt.value_option metavar default coerce
    (fun _ s -> Printf.sprintf "Invalid String '%s'" s)

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

(** read a debian Packages file - compressed or not *)
let read_deb ?filter ?(extras=[]) fname =
  let ch = Input.open_file fname in
  let l = Debian.Packages.input_raw_ch ?filter ~extras ch in
  let _ = Input.close_ch ch in
  l

(* fll = file list list
 * dll = deb packages list list 
 * cll = cudf package list list
 *)
let deb_load_list ?(extras=[]) ?(status=[]) dll =
  let pkglist = List.flatten dll in
  let pkglist = if status = [] then pkglist else Debian.Packages.merge status pkglist in
  let options = { 
    Debian.Debcudf.default_options with
    Debian.Debcudf.extras = extras;
    foreign = ["amd64";"i386";"arm";"armel"] 
  }
  in
  let tables = Debian.Debcudf.init_tables pkglist in
  let from_cudf (p,i) = (p,Debian.Debcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let cll = 
    List.map (fun l ->
      List.map (Debian.Debcudf.tocudf tables ~options) l
    ) dll
  in
  let preamble = Debian.Debcudf.preamble in
  (preamble,cll,from_cudf,to_cudf)
      
let pp_versions_table fmt (from_cudf, pkglist) =
  List.iter (fun pkg ->
    let (p,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in
    Format.fprintf fmt "%s=%d=%s@." p pkg.Cudf.version v
  ) pkglist

let eclipse_load_list ?(extras=[]) dll =
  let pkglist = List.flatten dll in
  let tables = Eclipse.Eclipsecudf.init_tables pkglist in
  let from_cudf (p,i) = (p, Eclipse.Eclipsecudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Eclipse.Eclipsecudf.get_cudf_version tables (p,v)) in
  let cll = 
    List.map (fun l ->
      List.map (Eclipse.Eclipsecudf.tocudf ~extras tables) l
    ) dll
  in
  let preamble = Eclipse.Eclipsecudf.preamble in
  (preamble,cll,from_cudf,to_cudf)
 
(** transform a list of debian control stanza into a cudf universe *)
let deb_load_universe ?(extras=[]) l =
  let (pr,cll,f,t) = deb_load_list ~extras [l] in
  (pr,Cudf.load_universe (List.flatten cll), f, t)

(* XXX double minded ... this code is kinda similar to the code in rpmcudf 
 * refactor or not refactor ? *)
(** transform a list of rpm control stanza into a cudf packages list *)
let rpm_load_list dll =
IFDEF HASRPM THEN
  let tables =  Rpm.Rpmcudf.init_tables (List.flatten dll) in
  let cll = 
    List.map (fun l -> 
      List.map (Rpm.Rpmcudf.tocudf tables) l 
    ) dll
  in
  (* Rpm.Rpmcudf.clear tables; *)
  let from_cudf (p,i) = (p,string_of_int i) in
  let to_cudf (p,v) = (p,Rpm.Rpmcudf.get_cudf_version tables (p,v)) in
  let preamble = Rpm.Rpmcudf.preamble in
  (preamble,cll,from_cudf,to_cudf)
ELSE
  failwith "librpm not available. re-configure with --with-rpm"
END

(** transform a list of rpm control stanza into a cudf universe *)
let rpm_load_universe l =
  let (pr,cll,f,t) = rpm_load_list [l] in
  (pr,Cudf.load_universe (List.flatten cll), f, t)

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
  (Cudf.default_preamble,[pkglist],from_cudf,to_cudf)

let cudf_load_universe file =
  let (pr,l,f,t) = cudf_load_list file in
  (pr,Cudf.load_universe (List.hd l), f, t)

(** return the name of the file *)
let unpack (_,(_,_,_,_,file),_) = file

(* Check that all uris are of type that is an instance of scheme *)
(* If yes return that instance of scheme, and the list of paths  *)
(* in uris.                                                      *)
(** parse a list of uris of the same type and return a cudf packages list *)
let parse_input ?(archs=[]) ?(extras=[]) (urilist : string list list) =
  let default = match List.flatten urilist with
    |uri::_ -> let (p,_,_) = Input.parse_uri uri in Some p
    |_ -> None
  in
  let filelist =
    List.map (fun uris ->
      List.filter_map (fun uri ->
        let (t,_,_) as p = Input.parse_uri uri in
        if Some t = default then Some p else None
      ) uris
    ) urilist
  in
  match default, filelist with
  |None,_ -> fatal "No input specified"
  |Some Url.Cudf,[[p]] when (unpack p) = "-" -> fatal "no stdin for cudf yet"
  |Some Url.Cudf,[[p]] -> cudf_load_list (unpack p)
  |Some Url.Cudf, l when (List.flatten l) = [] -> fatal "how do you know it's a cudf ?"
  |Some Url.Cudf, l -> 
      if List.length (List.flatten l) > 1 then
        warning "more then one cudf speficied on the command line";
      let p = List.hd (List.flatten l) in 
      cudf_load_list (unpack p)

  |Some Url.Deb,ll ->
      let dll = 
        List.map (fun l ->
          let filelist = List.map unpack l in
          Debian.Packages.input_raw ~archs filelist
        ) ll 
      in
      deb_load_list ~extras dll

  |Some Url.Eclipse, ll ->
      let dll = 
        List.map (fun l ->
          let filelist = List.map unpack l in
          Eclipse.Packages.input_raw filelist
        ) ll 
      in
      eclipse_load_list ~extras dll

  |Some Url.Hdlist, ll -> 
IFDEF HASRPM THEN
      let dll = 
        List.map (fun l ->
          let filelist = List.map unpack l in
          Rpm.Packages.Hdlists.input_raw filelist
        ) ll 
      in
      rpm_load_list dll
ELSE
    fatal "hdlist Not supported. re-configure with --with-rpm"
END

  |Some Url.Synthesis, ll -> 
IFDEF HASRPM THEN
      let dll = 
        List.map (fun l ->
          let filelist = List.map unpack l in
          Rpm.Packages.Synthesis.input_raw filelist
        ) ll 
      in
      rpm_load_list dll
ELSE
    fatal "synthesis input format not supported. re-configure with --with-rpm"
END
(*
  |Some (Url.Pgsql|Url.Sqlite), [((Url.Pgsql|Url.Sqlite) as dbtype,info,(Some query))] ->
IFDEF HASDB THEN
      let db = Db.Backend.init_database dbtype info (Idbr.parse_query query) in
      let l = Db.Backend.load_selection db (`All) in
      deb_load_list ~extras [l]
ELSE
    fatal "%s Not supported. re-configure with --with-??" (Url.scheme_to_string dbtype)
END
*)
    |Some s,_ -> fatal "%s Not supported" (Url.scheme_to_string s)
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
let load_list ?(archs=[]) ?(extras=[]) urilist =
  info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let u = parse_input ~archs ~extras urilist in
  Util.Timer.stop timer u
;;

(** parse and merge a list of files into a cudf universe *)
let load_universe ?(archs=[]) ?(extras=[]) uris =
  info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let (pr,cll,f,t) = parse_input ~archs ~extras [uris] in
  let u = (pr,Cudf.load_universe (List.flatten cll), f, t) in
  Util.Timer.stop timer u
;;

let if_application ?(alternatives=[]) filename main =
  let open Filename in
  let normalize f = 
    try chop_extension(basename f) 
    with Invalid_argument _ -> (basename f) 
  in
  let names = List.map normalize (filename::alternatives) in
  let invoked_as = normalize Sys.argv.(0) in
  if List.exists ((=) invoked_as) names then main ()
  else begin
    Printf.eprintf "you are using %s as a module and not as an executable\n" Sys.argv.(0);
    Printf.eprintf "%s can be run as an exactable if named : %s\n" Sys.argv.(0) 
    (ExtString.String.join " , " names)
  end
