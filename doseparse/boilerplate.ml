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

include Util.Logging(struct let label = __FILE__ end) ;;

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

let vpkg_option ?default ?(metavar = "VPKG") () =
  let parse_vpkg s = 
    let _loc = Debian.Format822.dummy_loc in
    Debian.Packages.parse_vpkg (_loc,s)
  in
  OptParse.Opt.value_option metavar default
  parse_vpkg (fun _ s -> Printf.sprintf "invalid vpackage '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (= v)" *)
let vpkglist_option ?default ?(metavar = "VPKGLST") () =
  let parse_vpkglist s = 
    let _loc = Debian.Format822.dummy_loc in
    Debian.Packages.parse_vpkglist (_loc,s)
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "invalid vpackage list '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (= v)" *)
let pkglist_option ?default ?(metavar = "PKGLST") () =
  let parse_vpkglist s = 
    let _loc = Debian.Format822.dummy_loc in
    List.map (function
      |((n,a),Some("=",v)) -> (n,a,v)
      |_ -> raise (Debian.Packages.ParseError (s,""))
    ) (Debian.Packages.parse_vpkglist (_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "invalid package list '%s'" s)
;;

(* *************************************** *)

let incr_str_list ?(default=Some []) ?(metavar = "STR") =
  let acc = ref [] in 
  let coerce s = acc := s :: !acc ; !acc in
  fun () ->
  OptParse.Opt.value_option metavar default coerce 
  (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* this is a ,-separated list of strings *)
let str_list_option ?default ?(metavar = "STRLST") =
  let sep = "," in
  let coerce s = ExtString.String.nsplit s sep in
  fun () ->
    OptParse.Opt.value_option metavar default coerce
    (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* *************************************** *)

module MakeOptions(O : Ot) = struct
  open OptParse ;;

  let verbose = StdOpt.incr_option ()
  let quiet = StdOpt.store_true ()
  let progress = StdOpt.store_true ()
  let timers = StdOpt.store_true ()
  let options = O.options ~version:VersionInfo.version () ;;

  open OptParser ;;
  add options ~short_name:'v' ~long_name:"verbose" ~help:"print additional information" verbose;
  add options ~long_name:"progress" ~help:"print progress bars" progress;
  add options ~long_name:"timers" ~help:"print timing information" timers;
  add options ~long_name:"quiet" ~help:"do no print any messages" quiet;

end

type options =
  |Deb of Debian.Debcudf.options
  |Eclipse of Debian.Debcudf.options
  |Edsp of Debian.Debcudf.options
  |Csw
  |Rpm
  |Cudf

module MakeDistribOptions(O : sig val options : OptParse.OptParser.t end) = struct
  open OptParse ;;

  let deb_native_arch = StdOpt.str_option ()
  let deb_foreign_archs = str_list_option ()
  let deb_target_arch = StdOpt.str_option ()
  let deb_ignore_essential = StdOpt.store_true ()

  let set_deb_options () =
    let native =
      if Opt.is_set deb_native_arch then
        Opt.get deb_native_arch
      else ""
    in
    let foreign = 
      if Opt.is_set deb_foreign_archs then
        Opt.get deb_foreign_archs 
      else []
    in
    let target =
      if Opt.is_set deb_target_arch then
        Opt.get deb_target_arch
      else native
    in
    {
      Debian.Debcudf.default_options with
      Debian.Debcudf.native = native;
      foreign = foreign;
      target = target;
      ignore_essential = Opt.get deb_ignore_essential
    }
  ;;

  let get_deb_buildarchs opt =
    if opt.Debian.Debcudf.native = "" then 
      fatal "you must specify at least the native architecture" ;
    opt.Debian.Debcudf.target
  ;;

  let set_default_options = function
    |`Deb -> Some (
      Deb { 
        Debian.Debcudf.default_options with
        Debian.Debcudf.ignore_essential = true
      })
    |`Edsp -> Some (
      Edsp { 
        Debian.Debcudf.default_options with
        Debian.Debcudf.ignore_essential = true
      })
    |`Eclipse -> Some (Eclipse Debian.Debcudf.default_options)
    |_ -> None

  let set_options = function
    |`Deb -> Some (Deb (set_deb_options ()))
    |`Edsp -> Some (Edsp (set_deb_options ()))
    |`Eclipse -> Some (Eclipse Debian.Debcudf.default_options)
    |_ -> None
  ;;

  open OptParser ;;
  let deb_group = add_group O.options "Debian Specific Options" in
  add O.options ~group:deb_group ~long_name:"deb-native-arch" ~help:"Native architecture" deb_native_arch;
  add O.options ~group:deb_group ~long_name:"deb-host-arch" ~help:"XCompile Target architecture" deb_target_arch;
  add O.options ~group:deb_group ~long_name:"deb-foreign-archs" ~help:"Foreign architectures" deb_foreign_archs;
  add O.options ~group:deb_group ~long_name:"deb-ignore-essential" ~help:"Ignore Essential Packages" deb_ignore_essential;

(*  let rpm_group = add_group options "Rpm Specific Options" in
    let eclipse_group = add_group options "Eclipse Specific Options" in
*)
end

let enable_debug = function
  |0 -> () (* only warning messages : default *)
  |1 -> Util.Info.all_enabled ()
  |_ ->
      begin
        Util.Info.all_enabled () ;
        Util.Debug.all_enabled ()
      end
;;

let all_quiet t =
  if t then begin
    Util.Info.all_disabled ();
    Util.Warning.all_disabled ();
    Util.Debug.all_disabled ();
    List.iter Util.Progress.disable (Util.Progress.available ())
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
  Debian.Packages.input_raw ?filter ~extras [fname]

(* fll = file list list
 * dll = deb packages list list 
 * cll = cudf package list list
 *)
let deb_load_list options ?(status=[]) dll =
  let pkglist = List.flatten dll in
  let pkglist = if status = [] then pkglist else Debian.Packages.merge status pkglist in
  let tables = Debian.Debcudf.init_tables pkglist in
  let from_cudf (p,i) = (p,Debian.Debcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let cll = 
    List.map (fun l ->
      (* XXX this is stupid and slow *)
      List.map (Debian.Debcudf.tocudf tables ~options) (Debian.Packages.merge status l)
    ) dll
  in
  let preamble = Debian.Debcudf.preamble in
  (preamble,cll,from_cudf,to_cudf)
      
let pp_versions_table fmt (from_cudf, pkglist) =
  List.iter (fun pkg ->
    let (p,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in
    Format.fprintf fmt "%s=%d=%s@." p pkg.Cudf.version v
  ) pkglist

let eclipse_load_list options dll =
  let extras = [] in
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
 
let csw_load_list dll =
  let pkglist = List.flatten dll in
  let tables = Csw.Cswcudf.init_tables pkglist in
  let from_cudf (p,i) = (p, Csw.Cswcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Csw.Cswcudf.get_cudf_version tables (p,v)) in
  let cll = 
    List.map (fun l ->
      List.map (Csw.Cswcudf.tocudf tables) l
    ) dll
  in
  let preamble = Csw.Cswcudf.preamble in
  (preamble,cll,from_cudf,to_cudf)
 
let edsp_load_list options file =
  let archs = 
    if options.Debian.Debcudf.native <> "" then
      options.Debian.Debcudf.native :: options.Debian.Debcudf.foreign 
    else []
  in
  let (_,pkglist) = Debian.Edsp.input_raw ~archs file in
  let tables = Debian.Debcudf.init_tables pkglist in
  let preamble =
    let l = List.map snd Debian.Edsp.extras_tocudf in
    Common.CudfAdd.add_properties Debian.Debcudf.preamble l
  in  
  let univ = Hashtbl.create (2*(List.length pkglist)-1) in
  let cudfpkglist =
    List.filter_map (fun pkg ->
      let p = Debian.Edsp.tocudf tables ~options pkg in
      if not(Hashtbl.mem univ (p.Cudf.package,p.Cudf.version)) then begin
        Hashtbl.add univ (p.Cudf.package,p.Cudf.version) pkg;
        Some p
      end else begin
        warning "Duplicated package (same version, name and architecture) : (%s,%s,%s)"
          pkg.Debian.Packages.name pkg.Debian.Packages.version pkg.Debian.Packages.architecture;
        None
      end
    ) pkglist
  in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let from_cudf (p,i) = (p, Debian.Debcudf.get_real_version tables (p,i)) in
  (preamble,[cudfpkglist;[]],from_cudf,to_cudf)

let edsp_load_universe options file =
  let (pr,l,f,t) = edsp_load_list options file in
  (pr,Cudf.load_universe (List.hd l), f, t)

(** transform a list of debian control stanza into a cudf universe *)
let deb_load_universe options l =
  let (pr,cll,f,t) = deb_load_list options [l] in
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
  |Cudf.Constraint_violation _ as exn -> begin
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

let cudf_load_list file =
  let preamble, pkglist =
    match parse_cudf file with
    |None, pkglist, _ -> Cudf.default_preamble, pkglist
    |Some p , pkglist, _ -> p, pkglist
  in
  let from_cudf (p,i) = (p,string_of_int i) in
  let to_cudf (p,v) = (p,int_of_string v) in
  (preamble,[pkglist;[]],from_cudf,to_cudf)

let cudf_load_universe file =
  let (pr,l,f,t) = cudf_load_list file in
  (pr,Cudf.load_universe (List.hd l), f, t)

(** return the name of the file *)
let unpack (_,(_,_,_,_,file),_) = file

let deb_parse_input options ?(status=[]) urilist =
  let archs = 
    if options.Debian.Debcudf.native <> "" then
      options.Debian.Debcudf.native :: options.Debian.Debcudf.foreign 
    else []
  in
  let dll = 
    List.map (fun l ->
      let filelist = List.map unpack l in
      Debian.Packages.input_raw ~archs filelist
    ) urilist
  in
  deb_load_list options ~status dll

let eclipse_parse_input options urilist =
  let dll = 
    List.map (fun l ->
      let filelist = List.map unpack l in
      Eclipse.Packages.input_raw filelist
    ) urilist
  in
  eclipse_load_list options dll

let csw_parse_input urilist =
  let dll = 
    List.map (fun l ->
      let filelist = List.map unpack l in
      Csw.Packages.input_raw filelist
    ) urilist
  in
  csw_load_list dll

let cudf_parse_input urilist =
  match urilist with
  |[[p]] when (unpack p) = "-" -> fatal "no stdin for cudf yet"
  |[[p]] -> cudf_load_list (unpack p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more then one cudf speficied on the command line";
    let p = List.hd (List.flatten l) in 
    cudf_load_list (unpack p)
;;

let edsp_parse_input options urilist =
  match urilist with
  |[[p]] when (unpack p) = "-" -> fatal "no stdin for edsp yet"
  |[[p]] -> edsp_load_list options (unpack p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more then one cudf speficied on the command line";
    let p = List.hd (List.flatten l) in 
    edsp_load_list options (unpack p)
;;

(* Check that all uris are of type that is an instance of scheme *)
(* If yes return that instance of scheme, and the list of paths  *)
(* in uris.                                                      *)
(** parse a list of uris of the same type and return a cudf packages list *)
let parse_input ?(options=None) urilist =
  let filelist = List.map (List.map Input.parse_uri) urilist in
  match Input.guess_format urilist, options with
  |`Cudf, None -> cudf_parse_input filelist

  |`Deb, None -> deb_parse_input Debian.Debcudf.default_options filelist
  |`Eclipse, None -> eclipse_parse_input Debian.Debcudf.default_options filelist

  |`Deb, Some (Deb opt) -> deb_parse_input opt filelist
  
  |`Edsp, Some (Edsp opt) -> edsp_parse_input opt filelist
  |`Edsp, None -> edsp_parse_input Debian.Debcudf.default_options filelist

  |`Eclipse, Some (Eclipse opt) -> eclipse_parse_input opt filelist

  |`Csw, None -> csw_parse_input filelist

  |`Hdlist, None -> 
IFDEF HASRPM THEN
      let dll = 
        List.map (fun l ->
          let filelist = List.map unpack l in
          Rpm.Packages.Hdlists.input_raw filelist
        ) filelist 
      in
      rpm_load_list dll
ELSE
    fatal "hdlist Not supported. re-configure with --with-rpm"
END

  |`Synthesis, None -> 
IFDEF HASRPM THEN
      let dll = 
        List.map (fun l ->
          let filelist = List.map unpack l in
          Rpm.Packages.Synthesis.input_raw filelist
        ) filelist
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
    |s,_ -> fatal "%s Not supported" (Url.scheme_to_string s)
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

(** return a list of Debian packages from a debian source file *)
let deb_load_source ?(profiles=false) ?(noindep=false) target builddeparch sourcefile =
  let l = Debian.Sources.input_raw ~archs:[target] [sourcefile] in
  Debian.Sources.sources2packages ~noindep ~profiles builddeparch l
;;

(** parse and merge a list of files into a cudf package list *)
let load_list ?(options=None) urilist =
  info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let u = parse_input ~options urilist in
  Util.Timer.stop timer u
;;

(** parse and merge a list of files into a cudf universe *)
let load_universe ?(options=None) uris =
  info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let (pr,cll,f,t) = parse_input ~options [uris] in
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
