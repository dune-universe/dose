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
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf)
      
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
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf)
 
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
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf)
 
let edsp_load_list options file =
  let (request,pkglist) = Debian.Edsp.input_raw file in
  let (native_arch,foreign_archs) =
    StdUtils.get_architectures
      request.Debian.Edsp.architecture
      request.Debian.Edsp.architectures
      options.Debian.Debcudf.native
      (match options.Debian.Debcudf.foreign with [] -> None | l -> Some l)
  in
  let options = { 
    options with 
    Debian.Debcudf.native = native_arch;
    Debian.Debcudf.foreign = foreign_archs
  } in
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
  let request = Debian.Edsp.requesttocudf tables (Cudf.load_universe cudfpkglist) request in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let from_cudf (p,i) = (p, Debian.Debcudf.get_real_version tables (p,i)) in
  (preamble,[cudfpkglist;[]],request,from_cudf,to_cudf)

let edsp_load_universe options file =
  let (pr,l,r,f,t) = edsp_load_list options file in
  (pr,Cudf.load_universe (List.hd l), r, f, t)

(** transform a list of debian control stanza into a cudf universe *)
let deb_load_universe options l =
  let (pr,cll,r,f,t) = deb_load_list options [l] in
  (pr,Cudf.load_universe (List.flatten cll), r, f, t)

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
  let request = Cudf.default_request in
  (preamble,cll,request,from_cudf,to_cudf)
ELSE
  failwith "librpm not available. re-configure with --with-rpm"
END

(** transform a list of rpm control stanza into a cudf universe *)
let rpm_load_universe l =
  let (pr,cll,r,f,t) = rpm_load_list [l] in
  (pr,Cudf.load_universe (List.flatten cll), r, f, t)

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

let cudfv_load_list options file =
  let preamble, pkglist ,request =
    match parse_cudf file with
    |None, pkglist, None -> Cudf.default_preamble, pkglist, Cudf.default_request
    |None , pkglist, Some req -> Cudf.default_preamble, pkglist, req
    |Some p , pkglist, None -> p, pkglist, Cudf.default_request
    |Some p , pkglist, Some req -> p, pkglist, req
  in
  if options.Cudfv.Cudfvcudf.cudfv then
  let from_cudf (p,i) = (p,string_of_int i) in
  let to_cudf (p,v) = (p,int_of_string v) in
  (preamble,[pkglist;[]],request,from_cudf,to_cudf)
  else
  let tables = Cudfv.Cudfvcudf.init_tables options pkglist file in
  let from_cudf (p,i) = (p, Cudfv.Cudfvcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p, Cudfv.Cudfvcudf.get_cudf_version tables (p,v)) in 
  (preamble,[pkglist;[]],request,from_cudf,to_cudf)

let cudf_load_list file =
  let preamble, pkglist ,request =
    match parse_cudf file with
    |None, pkglist, None -> Cudf.default_preamble, pkglist, Cudf.default_request
    |None , pkglist, Some req -> Cudf.default_preamble, pkglist, req
    |Some p , pkglist, None -> p, pkglist, Cudf.default_request
    |Some p , pkglist, Some req -> p, pkglist, req
  in
  let from_cudf (p,i) = (p,string_of_int i) in
  let to_cudf (p,v) = (p,int_of_string v) in
  (preamble,[pkglist;[]],request,from_cudf,to_cudf)

let cudf_load_universe file =
  let (pr,l,r,f,t) = cudf_load_list file in
  (pr,Cudf.load_universe (List.hd l), r, f, t)

(** return the name of the file *)
let unpack (_,(_,_,_,_,file),_) = file

let deb_parse_input options ?(status=[]) urilist =
  let archs = 
    if not(Option.is_none options.Debian.Debcudf.native) then
      (Option.get options.Debian.Debcudf.native) :: options.Debian.Debcudf.foreign 
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

let cudfv_parse_input options urilist =
  match urilist with
  |[[p]] when (unpack p) = "-" -> fatal "no stdin for cudf yet"
  |[[p]] -> cudf_load_list (unpack p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more then one cudf specified on the command line";
    let p = List.hd (List.flatten l) in 
    cudfv_load_list options (unpack p)


let cudf_parse_input urilist =
  match urilist with
  |[[p]] when (unpack p) = "-" -> fatal "no stdin for cudf yet"
  |[[p]] -> cudf_load_list (unpack p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more then one cudf specified on the command line";
    let p = List.hd (List.flatten l) in 
    cudf_load_list (unpack p)
;;

let edsp_parse_input options urilist =
  match urilist with
  |[[p]] when (unpack p) = "-" -> fatal "no stdin for edsp yet"
  |[[p]] -> edsp_load_list options (unpack p)
  |l ->
    if List.length (List.flatten l) > 1 then
      warning "more then one cudf specified on the command line";
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

  |`Cudfv, None -> cudfv_parse_input Cudfv.Cudfvcudf.default_options filelist

  |`Deb, None -> deb_parse_input Debian.Debcudf.default_options filelist
  |`Eclipse, None -> eclipse_parse_input Debian.Debcudf.default_options filelist

  |`Deb, Some (StdOptions.Deb opt) -> deb_parse_input opt filelist
  
(*  |`Edsp, Some (StdOptions.Edsp opt) -> edsp_parse_input opt filelist *)
  |`Edsp, _ -> edsp_parse_input Debian.Debcudf.default_options filelist

  |`Eclipse, Some (StdOptions.Eclipse opt) -> eclipse_parse_input opt filelist

  |`Csw, None -> csw_parse_input filelist

  |`Cudfv, Some (StdOptions.Cudfv opt) -> cudfv_parse_input opt filelist

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
   standard@rpm
;;

(** return a list of Debian packages from a debian source file *)
let deb_load_source ?filter ?(profiles=[]) ?(noindep=false) buildarch hostarch sourcefile =
  let l = Debian.Sources.input_raw ?filter ~archs:[hostarch] [sourcefile] in
  Debian.Sources.sources2packages ~noindep ~profiles buildarch hostarch l
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
  let (pr,cll,r,f,t) = parse_input ~options [uris] in
  let u = (pr,Cudf.load_universe (List.flatten cll), r, f, t) in
  Util.Timer.stop timer u
;;

