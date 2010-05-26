(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Common
open Debian

module Deb = Debian.Packages

module L = Xml.LazyList 

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let outdir = StdOpt.str_option ()
  let problemid = StdOpt.str_option ()

  let description = "Convert Debian Dudf files to Cudf format"
  let options = OptParser.make ~description:description ()

  open OptParser ;;
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'o' ~long_name:"outdir" ~help:"Output directory" outdir;
  add options                 ~long_name:"id" ~help:"Problem id" problemid;
end

(* ========================================= *)

module AptPref = struct

  type criteria = {
    origin : string option;
    component : string option;
    release_version : string option;
    label : string option;
    archive : string option
  }

  let print_criteria = function
    { origin = o ;
      component = c ;
      release_version = v ;
      label = l ;
      archive = a
    } -> 
      let f = Option.may (Printf.eprintf "%s\n") in
      f o ; f c ; f v ; f l ; f a
  ;;

  type priority = int
  type generic = criteria
  type specific = {
    name : string ;
    version : string option ;
    criteria : criteria ;
  }

  type preferences = {
    target_release : string option ;
    specific : (specific * priority) list;
    generic  : (generic * priority) list
  }

  let map_criteria criteria = function
    |"v",v -> {criteria with release_version = Some v }
    |"c",v -> {criteria with component = Some v}
    |"o",v -> {criteria with origin = Some v}
    |"l",v -> {criteria with label = Some v}
    |"a",v -> {criteria with archive = Some v}
    |_,_ -> assert false

  let dummypref = { target_release = None ; specific = [] ; generic = [] }
  let dummycriteria = {
    origin = None ; component = None ; 
    release_version = None ; 
    label = None ; archive = None
  }
  let dummyspec = { name = "undef" ; version = None ; criteria = dummycriteria }

  let mapf preferences = function
    { Debian.Apt.Pref.package = pkg ; pin = pin ; pin_priority = priority } ->
      match pkg with
      |Debian.Apt.Pref.Star ->
          begin match pin with
          |Debian.Apt.Pref.Version _ -> assert false
          |Debian.Apt.Pref.Origin origin -> begin
              Printf.eprintf "Warning : origin is not currectly supported\n" ;
              let c = { dummycriteria with origin = Some origin } in
              {preferences with generic = (c, priority) :: preferences.generic}
          end
          |Debian.Apt.Pref.Release criteria -> 
              let c = List.fold_left map_criteria dummycriteria criteria in
              {preferences with generic = (c, priority) :: preferences.generic}
          end
      |Debian.Apt.Pref.Package name ->
          begin match pin with
          |Debian.Apt.Pref.Version version -> 
              let s = { dummyspec with name = name ; version = Some version } in
              {preferences with specific = (s, priority) :: preferences.specific }
          |Debian.Apt.Pref.Origin origin -> begin
              Printf.eprintf "Warning : origin is not currectly supported\n" ;
              let c = { dummycriteria with origin = Some origin } in
              let s = { dummyspec with name = name ; criteria = c} in
              {preferences with specific = (s, priority) :: preferences.specific}
          end
          |Debian.Apt.Pref.Release criteria ->
              let c = List.fold_left map_criteria dummycriteria criteria in
              let s = { dummyspec with name = name ; criteria = c } in
              {preferences with specific = (s, priority) :: preferences.specific}
          end
  ;;

  let parse ?tr s =
    let ch = IO.input_string s in
    let l = Debian.Apt.parse_preferences_in (fun x -> x) ch in
    let pref = List.fold_left mapf dummypref l in
    { pref with target_release = tr }

  let match_criteria constr c =
       (c.origin = None || c.origin = constr.origin) 
    && (c.release_version = None || c.release_version = constr.release_version) 
    && (c.component = None || c.component = constr.component)
    && (c.archive = None || c.archive = constr.archive)
    && (c.label = None || c.label = constr.label)
  ;;

  let match_version constr c = 
    if (Option.is_none constr) || (Option.is_none c) then false
    else
      let s = Str.global_replace (Str.regexp "\\*") "\\.*" (Option.get constr) in
      let version_re = Str.regexp s in
      Str.string_match version_re (Option.get c) 0
  ;;

  let match_specific constr c =
       (c.name = constr.name)
    && ((c.version = None) 
          || (c.version = constr.version)
          || (match_version c.version constr.version) )
    && (match_criteria constr.criteria c.criteria)
  ;;

  let find_specific constr l = 
    List.find_all (fun (c,_) -> match_specific constr c) l

  let find_generic constr l = 
    List.find_all (fun (c,_) -> match_criteria constr c) l

  let find_max l = List.fold_left (fun a (_,b) -> max a b) 0 l

  let get_priority pref info pkg =
    let number = Cudf.lookup_package_property pkg "number" in
    let constr = { dummyspec with name = pkg.Cudf.package ; version = Some number } in
    match (find_specific constr pref.specific, info) with
    |[], None -> None
    |[], Some info ->
        begin
          let constr = {dummycriteria with archive = Some(info.Debian.Release.suite)} in
          match find_generic constr pref.generic with
          |[] -> None 
          |l -> Some(find_max l)
        end
    |l,_ -> Some(find_max l)

  let assign_priority preferences info package =
    match get_priority preferences info package with
    |None ->
      begin match preferences.target_release,info with
      |(Some _, None) | (None,_) ->
          if package.Cudf.installed then 100 else 500
      |Some tr, Some info ->
          if package.Cudf.installed then 100 else
          if not (tr = info.Debian.Release.suite) then 500
          else 990
      end
    |Some p -> p

end

(* ========================================= *)

let make_universe pl =
  let fl = ref [] in
  let (packagelist,releaselist) =
    List.partition (function 
      |("apt",_,_,_) -> true
      |("apt-release",_,_,_) -> false
      |_ -> assert false
    ) pl
  in
  let universe = 
    List.flatten (
      List.map (fun (_,fname,_,cdata) ->
        let i = Str.search_backward (Str.regexp "_Release") fname (String.length fname) in
        let s = Str.string_before fname i in
        let release = 
          let ch = IO.input_string cdata in
          let r = Debian.Release.parse_release_in ch in
          let _ = IO.close_in ch in
          r
        in
        let cl =
          List.find_all (fun (_,fname,_,_) ->
            Str.string_match (Str.regexp ("^"^s^".*_Packages$")) fname 0
          ) packagelist
        in
        List.map (fun (_,fname,_,cdata) -> fl := fname :: !fl ; (release,cdata)) cl
      ) releaselist
    )
  in
  let without_release = 
    List.map (fun (_,fname,_,cdata) ->
      Printf.eprintf "Warning : Package List without Release. %s\n" fname;
      (Debian.Release.default_release,cdata)
    ) (List.find_all (fun (_,fname,_,_) -> not(List.mem fname !fl)) packagelist) ;
  in
  universe @ without_release
;;

let has_children nodelist tag =
  try match nodelist with
    |t::_ when (Xml.tag t) = tag -> true
    |_ -> false
  with Xml.Not_element(_) -> false
;;

let parsepackagelist = function
  |(Some "apt",Some fname,url,[inc]) when has_children [inc] "include" ->
      let href = Xml.attrib inc "href" in
      ("apt",fname,url, Dudfxml.pkgget ~compression:Dudfxml.Bz2 ~fname href)
  |(Some "apt-release",Some fname,url,[inc]) when has_children [inc] "include" ->
      let href = Xml.attrib inc "href" in
      ("apt-release",fname,url, Dudfxml.pkgget fname href)
  |(Some t,Some fname,url,[cdata]) -> (t,fname,url,Xml.cdata cdata)
  |(Some t,Some fname,url,[]) -> (t,fname,url,"")
  |(Some t,Some fname,url,_) ->
      (Printf.eprintf "Warning : Unknown format for package-list element %s %s\n" t fname; exit 1)
  |_ -> assert false
;;

(* ========================================= *)

open Dudfxml.XmlDudf

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  Random.self_init () ;
  let input_file =
    match OptParse.OptParser.parse_argv Options.options with
    |[h] -> h
    |_ -> (Printf.eprintf "too many arguments" ; exit 1)
  in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  Util.print_info "parse xml";

  let id x = x in

  let dudfdoc = Dudfxml.parse input_file in
  let uid = dudfdoc.uid in
  let status =
    match dudfdoc.problem.packageStatus.st_installer with
    |[status] -> Xml.fold (fun a x -> a^(Xml.cdata x)) "" status
    |_ -> Printf.eprintf "Warning: wrong status" ; ""
  in
  let packagelist = 
    let l = List.map (fun pl -> parsepackagelist pl) dudfdoc.problem.packageUniverse in
    make_universe l
  in
  let action = dudfdoc.problem.action in
  let preferences = AptPref.parse dudfdoc.problem.desiderata in

  Util.print_info "convert to dom ... ";

  let infoH = Hashtbl.create 1031 in
  let extras_property = [
    ("Size", ("size", `Nat (Some 0)));
    ("Installed-Size", ("installedsize", `Nat (Some 0)));
    ("Maintainer", ("maintainer", `String None))]
  in
  let extras = List.map fst extras_property in

  Util.print_info "parse all packages";
  let all_packages =
    List.fold_left (fun acc (release,contents) ->
      let ch = IO.input_string contents in
      let l = Deb.parse_packages_in ~extras:extras id ch in
      let _ = IO.close_in ch in
      List.fold_left (fun s pkg -> 
        Hashtbl.add infoH (pkg.Deb.name,pkg.Deb.version) release ;
        Deb.Set.add pkg s
      ) acc l
    ) Deb.Set.empty packagelist
  in

  Util.print_info "installed packages";
  let installed_packages =
    let ch = IO.input_string status in
    let l = Deb.parse_packages_in ~extras:extras id ch in
    let _ = IO.close_in ch in
    List.fold_left (fun s pkg -> Deb.Set.add pkg s) Deb.Set.empty l
  in

  Util.print_info "union";
  let l = Deb.Set.elements (Deb.Set.union all_packages installed_packages) in
  let tables = Debian.Debcudf.init_tables l in

  let installed =
    let h = Hashtbl.create 1031 in
    Deb.Set.iter (fun pkg ->
      Hashtbl.add h (pkg.Deb.name,pkg.Deb.version) ()
    ) installed_packages
    ;
    h
  in
  
  let add_extra (k,v) pkg =
    { pkg with Cudf.pkg_extra = (k,v) :: pkg.Cudf.pkg_extra } in

  Util.print_info "convert";
  let pl =
    List.map (fun pkg ->
      let inst = Hashtbl.mem installed (pkg.Deb.name,pkg.Deb.version) in
      let info = try Some(Hashtbl.find infoH (pkg.Deb.name,pkg.Deb.version)) with Not_found -> None in
      let cudfpkg = Debcudf.tocudf tables ~extras:extras_property ~inst:inst pkg in
      let priority = AptPref.assign_priority preferences info cudfpkg in
      let cudfpkg = add_extra ("priority", `Int priority) cudfpkg in
      cudfpkg
    ) l
  in

  let universe = Cudf.load_universe pl in

  Util.print_info "request";
  let request =
    let mapver = function
      |`Pkg p -> (p,None)
      |`PkgVer (p,v) -> begin
          try (p,Some(`Eq,Debcudf.get_cudf_version tables (p,v)))
          with Not_found -> failwith (Printf.sprintf "There is no version %s of package %s" p v)
      end
      |`PkgDst (p,d) ->
          try
            let l = Cudf.lookup_packages universe p in
            let pkg = List.find (fun pkg ->
                let number = Cudf.lookup_package_property pkg "number" in
                let info = Hashtbl.find infoH (pkg.Cudf.package,number) in
                info.Debian.Release.suite = d
              ) l
            in
            let number = Cudf.lookup_package_property pkg "number" in
            (pkg.Cudf.package,Some(`Eq,Debcudf.get_cudf_version tables (pkg.Cudf.package,number)))
          with Not_found ->
            failwith (Printf.sprintf "There is no package %s in release %s " p d)
    in
    let request_id =
      if OptParse.Opt.is_set Options.problemid then OptParse.Opt.get Options.problemid
      else if uid <> "" then uid
      else (string_of_int (Random.bits ()))
    in
    match Debian.Apt.parse_request_apt action with
    |Debian.Apt.Upgrade (Some (suite))
    |Debian.Apt.DistUpgrade (Some (suite)) -> 
        let il = Deb.Set.fold (fun pkg acc -> `PkgDst (pkg.Deb.name,suite) :: acc) installed_packages [] in
        let l = List.map mapver il in
        { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; }
    |Debian.Apt.Install l ->
        let l = List.map mapver l in
        { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; } 
    |Debian.Apt.Remove l -> 
        let l = List.map (fun (`Pkg p) -> (p,None) ) l in
        { Cudf.request_id = request_id ; install = [] ; remove = l ; upgrade = [] ; req_extra = [] ;}
    |Debian.Apt.Upgrade None -> 
        { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = [] ; req_extra = [] ; }
    |Debian.Apt.DistUpgrade None -> 
        { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = [] ; req_extra = [] ; }
  in

  Util.print_info "dump";
  let oc =
    if OptParse.Opt.is_set Options.outdir then begin
      let dirname = OptParse.Opt.get Options.outdir in
      let file =
        let s = Filename.basename input_file in
        try Filename.chop_extension s with Invalid_argument _ -> s
      in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
      open_out (Filename.concat dirname (file^".cudf"))
    end else stdout
  in
  let preamble =
    let p = ("priority",(`Int (Some 500))) in
    let l = List.map snd extras_property in
    CudfAdd.add_properties Debcudf.preamble (p::l)
  in
  Cudf_printer.pp_cudf (Format.formatter_of_out_channel oc) (preamble, universe, request)
;;

main ();;

