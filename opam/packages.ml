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
  upgrade : Pef.Packages_types.vpkg list;
  dist_upgrade : bool;
  switch : string;
  switches : string list;
  profiles : string list;
  preferences: string;
}

let default_request = {
  install = [];
  remove = [];
  upgrade = [];
  dist_upgrade = false;
  switch = "";
  switches = [];
  profiles = [];
  preferences = "";
}

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
  if matchswitch (switch::switches) al && matchos profiles pl then [v]
  else []

let vpkglist_filter options l =
  List.flatten (List.map (select options) l)

let vpkgformula_filter options ll =
  List.filter_map (fun l ->
    match vpkglist_filter options l with
    |[] -> None
    |l -> Some l
  ) ll

let parse_req = Pef.Packages.lexbuf_wrapper Pef.Packages_parser.vpkglist_top

let parse_request_stanza par =
  try
    {
      install = Pef.Packages.parse_s ~default:[] parse_req "install" par;
      remove = Pef.Packages.parse_s ~default:[] parse_req "remove" par;
      upgrade = Pef.Packages.parse_s ~default:[] parse_req "upgrade" par;
      dist_upgrade = Pef.Packages.parse_s ~default:false Pef.Packages.parse_bool "dist_upgrade" par;
      switch = Pef.Packages.parse_s ~required:true Pef.Packages.parse_string "switch" par;
      switches = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_string_list "switches" par;
      profiles = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_string_list "profiles" par;
      preferences = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "preferences" par;
    }
  with Pef.Packages.ParseError (cl,f,err) ->
    let c = "Parser Error in Preamble" in
    raise ( Pef.Packages.ParseError (c::cl,f,err) )

class package ?(name=("package",None)) ?(version=("version",None)) ?(depends=("depends",None))
    ?(conflicts=("conflicts",None)) ?(provides=("provides",None)) ?(depopts=("depopts",None)) 
    ?(switch=("switch",None)) ?(installedlist=("installed",None)) ?(build_depends=("build-depends",None)) 
    ?(base=("base",None)) ?(extras=([],None)) par = object
  
  inherit Pef.Packages.package ~name ~version ~depends ~conflicts ~provides ~recommends:depopts ~extras par

  val switch : (string * string list) =
    let parse = Pef.Packages.parse_s ~default:["all"] (Pef.Packages.parse_string_list ~rex:Pef.Packages.comma_regexp) in
    Pef.Packages.get_field_value ~parse ~par ~field:switch

  val installedlist : (string * string list) =
    let parse = Pef.Packages.parse_s ~default:[] (Pef.Packages.parse_string_list ~rex:Pef.Packages.comma_regexp) in
    Pef.Packages.get_field_value ~parse ~par ~field:installedlist

  val build_depends : (string * Pef.Packages_types.vpkgformula) =
    let parse = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_vpkgformula in
    Pef.Packages.get_field_value ~parse ~par ~field:build_depends

  val base : (string * bool) =
    let parse = Pef.Packages.parse_s ~default:false Pef.Packages.parse_bool in
    Pef.Packages.get_field_value ~parse ~par ~field:base

  method switch = snd switch
  method installedlist = snd installedlist
  method build_depends = snd build_depends
  method base = snd base
  method depopts = snd recommends

  method pp oc =
    Pef.Printer.pp_string oc name;
    Pef.Printer.pp_string oc version;

    Pef.Printer.pp_string_list oc switch;
    Pef.Printer.pp_string_list oc installedlist;
    Pef.Printer.pp_bool oc base;

    Pef.Printer.pp_vpkglist oc provides;
    Pef.Printer.pp_vpkgformula oc depends;
    Pef.Printer.pp_vpkgformula oc recommends;
    Pef.Printer.pp_vpkglist oc conflicts;
    Printf.fprintf oc "\n"

end

(* a stanza is not considered if the intersection between the
active switch and the not available switches for a package is
empty *)
let parse_package_stanza ((switch,switches,profiles) as options) ?(extras=[]) par =
  try
    let pkg =
      let depends =
        let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepsformula in
        ("depends",Some (vpkgformula_filter options (f "depends" par)))
      in
      let depopts =
        let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepsformula in
        ("depopts",Some (vpkgformula_filter options (f "depopts" par)))
      in
      let conflicts =
        let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepslist in
        ("conflicts",Some (vpkglist_filter options (f "conflicts" par)))
      in
      let provides =
        let f = Pef.Packages.parse_s ~default:[] Pef.Packages.parse_builddepslist in
        ("provides",Some (vpkglist_filter options (f "provides" par)))
      in
      (* let extras = List.map (fun (f,v) -> (f,(Format822.dummy_loc,v))) extras in *)
      new package ~depends ~conflicts ~provides ~depopts ~extras:(extras,None) par
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
      let n = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_name "package" par in
      let v = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_version "version" par in
      warning "Ignoring Package (%s,%s) : %s" n v s; 
      None
    end
  |Pef.Packages.ParseError (cl,f,err) -> begin
      let n = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_name "package" par in
      let v = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_version "version" par in
      let c = Printf.sprintf "Parser Error in Package (%s,%s)" n v in
      raise ( Pef.Packages.ParseError (c::cl,f,err) )
  end

(* parse the entire file while filtering out unwanted stanzas.
 * Depopts is alsways false while parsing opam files *)
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

(* this function raise Pef.Packages.ParseError *)
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
  with 
  |Input.File_empty -> (default_request,[])
  |Pef.Packages.ParseError (cl,field,errmsg) ->
      fatal "Filename %s\n %s\n %s : %s" file (String.concat "\n " cl) field errmsg
;;
