(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
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

open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

type request = {
  install : Pef.Packages_types.vpkg list;
  remove : Pef.Packages_types.vpkg list;
  upgrade : bool;
  switch : string;
  switches : string list;
  profiles : string list;
  preferences: string;
}

let default_request = {
  install = [];
  remove = [];
  upgrade = false;
  switch = "";
  switches = [];
  profiles = [];
  preferences = "";
}

let parse_req (loc,s) = Pef.Packages.lexbuf_wrapper Pef.Packages_parser.vpkglist_top (loc,s)

let parse_request_stanza par =
  {
    install = Pef.Packages.parse_s ~opt:[] parse_req "install" par;
    remove = Pef.Packages.parse_s ~opt:[] parse_req "remove" par;
    upgrade = Pef.Packages.parse_s ~opt:false Pef.Packages.parse_bool "upgrade" par;
    switch = Pef.Packages.parse_s ~err:"(Missing active Switch)" Pef.Packages.parse_string "switch" par;
    switches = Pef.Packages.parse_s ~opt:[] Pef.Packages.parse_string_list "switches" par;
    profiles = Pef.Packages.parse_s ~opt:[] Pef.Packages.parse_string_list "profiles" par;
    preferences = Pef.Packages.parse_s ~opt:"" Pef.Packages.parse_string "preferences" par;
  }

class package ?(name=("package",None)) ?(version=("version",None)) ?(depends=("depends",None))
    ?(conflicts=("conflicts",None)) ?(provides=("provides",None)) ?(recommends=("recommends",None)) 
    ?(switch=("switch",None)) ?(installedlist=("installed",None)) ?(build_depends=("build-depends",None)) par = object
  
  inherit Pef.Packages.package ~name ~version ~depends ~conflicts ~provides ~recommends par

  val switch : string list =
    let f = Pef.Packages.parse_s ~opt:["all"] (Pef.Packages.parse_string_list ~rex:Pef.Packages.comma_regexp) in
    Pef.Packages.get_field_value f par switch

  val installedlist : string list =
    let f = Pef.Packages.parse_s ~opt:[] (Pef.Packages.parse_string_list ~rex:Pef.Packages.comma_regexp) in
    Pef.Packages.get_field_value f par installedlist

  val build_depends : Pef.Packages_types.vpkgformula =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkgformula in
    Pef.Packages.get_field_value f par build_depends

  method switch = switch
  method installedlist = installedlist
  method build_depends = build_depends

end

let addswitch (switch,switches) ((name,sw),constr) =
  match sw with
  |None -> [((name,Some switch),constr)]
  |Some "any" -> List.map (fun s -> ((name,Some s),constr)) switches
  |Some _ -> [((name,sw),constr)]

let matchswitch switches = function
  | [] -> true
  | ((true,_)::_) as al ->
    List.exists (fun (_,a) -> List.mem a switches) al
  | ((false,_)::_) as al ->
    List.for_all (fun (_,a) -> not(List.mem a switches)) al

let matchos profiles = function
  | [] -> true
  | ll ->
      List.exists (
        List.for_all (fun (c,p) ->
          c = (List.mem p profiles)
        )
      ) ll

let select (switch,switches,profiles) (v,al,pl) =
  if matchswitch (switch::switches) al && matchos profiles pl then
    Some (addswitch (switch,switches) v)
  else None

let vpkglist_filter options l =
  List.flatten (List.filter_map (select options) l)

let vpkgformula_filter options ll =
  List.filter_map (fun l ->
    match vpkglist_filter options l with
    |[] -> None
    |l -> Some l
  ) ll

(* a stanza is not considered if the intersection between the
active switch and the not available switches for a package is
empty *)
let parse_package_stanza ((switch,switches,profiles) as options) par =
  try
    let pkg =
      let depends =
        let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_builddepsformula in
        ("depends",Some (vpkgformula_filter options (f "depends" par)))
      in
      let conflicts =
        let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_builddepslist in
        ("donflicts",Some (vpkglist_filter options (f "donflicts" par)))
      in
      let provides =
        let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_builddepslist in
        ("drovides",Some (vpkglist_filter options (f "drovides" par)))
      in
      let recommends =
        let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_builddepsformula in
        ("decommends",Some (vpkgformula_filter options (f "decommends" par)))
      in
      new package ~depends ~conflicts ~recommends ~provides par
    in
    (* XXX here I could be a tiny bit faster by parsing the switch first and then all other
     * fields if necessary *)
    if List.mem "all" pkg#switch then Some pkg else
    if List.exists (fun s -> List.mem s pkg#switch) (switch::switches) then Some pkg
    else
      raise (Pef.Packages.IgnorePackage (
        Printf.sprintf
        "None of the active switches [%s] are available [%s]" 
        (ExtString.String.join "," (switch::switches))
        (ExtString.String.join "," pkg#switch)
        )
      )
  with 
  |Pef.Packages.IgnorePackage s -> begin
      let n = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_name "Package" par in
      let v = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_version "Version" par in
      warning "Ignoring Package (%s,%s) : %s" n v s; 
      None
    end
  |Pef.Packages.ParseError (f,s) -> begin
      let n = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_name "Package" par in
      let v = Pef.Packages.parse_s ~opt:"?" Pef.Packages.parse_version "Version" par in
      let err = Printf.sprintf "Parser Error in Package (%s,%s) : %s" n v s in
      raise ( Pef.Packages.ParseError (f,err) )
  end

(* parse the entire file while filtering out unwanted stanzas *)
let rec packages_parser ?(request=false) (req,acc) p =
  let options = (req.switch,req.switches,req.profiles) in
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> (req,acc) (* end of file *)
  |Some stanza when request = true ->
      let req = parse_request_stanza stanza in
      packages_parser (req,acc) p
  |Some stanza -> begin
    match (parse_package_stanza options stanza) with
    |None -> packages_parser (req,acc) p
    |Some st -> packages_parser (req,st::acc) p
  end
;;

let input_raw_ch ic =
  Format822.parse_from_ch (
    packages_parser ~request:true (default_request,[])
  ) ic
;;

let input_raw file =
  try
    let ch =
      match file with
      |"-" -> IO.input_channel stdin
      |_   -> Input.open_file file
    in
    let l = input_raw_ch ch in
    let _ = Input.close_ch ch in
    l
  with Input.File_empty -> (default_request,[])
;;
