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

type options =
  Pef.Packages_types.architecture * Pef.Packages_types.architecture list *
  Pef.Packages_types.buildprofile list

type request = {
  install : Pef.Packages_types.vpkg list;
  remove : Pef.Packages_types.vpkg list;
  upgrade : Pef.Packages_types.vpkg list;
  dist_upgrade : bool;
  preferences: string;
}

let default_request = {
  install = [];
  remove = [];
  upgrade = [];
  dist_upgrade = false;
  preferences = "";
}

let parse_req = Pef.Packages.lexbuf_wrapper Pef.Packages_parser.vpkglist_top

let parse_request_stanza par =
  try
    {
      install = Pef.Packages.parse_s ~default:[] parse_req "install" par;
      remove = Pef.Packages.parse_s ~default:[] parse_req "remove" par;
      upgrade = Pef.Packages.parse_s ~default:[] parse_req "upgrade" par;
      dist_upgrade = Pef.Packages.parse_s ~default:false Pef.Packages.parse_bool "dist_upgrade" par;
      preferences = Pef.Packages.parse_s ~default:"" Pef.Packages.parse_string "preferences" par;
    }
  with Format822.ParseError (cl,f,err) ->
    let c = "Parser Error in Preamble" in
    raise ( Format822.ParseError (c::cl,f,err) )

class package ?(name=("package",None)) ?(version=("version",None)) ?(depends=("depends",None))
    ?(conflicts=("conflicts",None)) ?(provides=("provides",None)) ?(recommends=("recommends",None)) 
    ?(extras=([],None)) par = object
  
  inherit Pef.Packages.package ~name ~version ~depends ~conflicts ~provides ~recommends ~extras par

  method pp oc =
    Pef.Printer.pp_string_wl oc name;
    Pef.Printer.pp_string_wl oc version;

    Pef.Printer.pp_vpkglist_wl oc provides;
    Pef.Printer.pp_vpkgformula_wl oc depends;
    Pef.Printer.pp_vpkgformula_wl oc recommends;
    Pef.Printer.pp_vpkglist_wl oc conflicts;
    Printf.fprintf oc "\n"

end

(* a stanza is not considered if the intersection between the
active switch and the not available switches for a package is
empty *)
let parse_package_stanza ?(extras=[]) par =
  try
    Some (new package par)
  with 
  |Pef.Packages.IgnorePackage s -> begin
      let n = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_name "package" par in
      let v = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_version "version" par in
      warning "Ignoring Package (%s,%s) : %s" n v s; 
      None
    end
  |Format822.ParseError (cl,f,err) -> begin
      let n = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_name "package" par in
      let v = Pef.Packages.parse_s ~default:"?" Pef.Packages.parse_version "version" par in
      let c = Printf.sprintf "Parser Error in Package (%s,%s)" n v in
      raise ( Format822.ParseError (c::cl,f,err) )
  end

(* parse the entire file while filtering out unwanted stanzas.
 * Depopts is alsways false while parsing opam files *)
let rec packages_parser ?(request=false) (req,acc) p =
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> (req,acc) (* end of file *)
  |Some stanza when request = true ->
      let req = parse_request_stanza stanza in
      packages_parser (req,acc) p
  |Some stanza -> begin
    match (parse_package_stanza stanza) with
    |None -> packages_parser (req,acc) p
    |Some st -> packages_parser (req,st::acc) p
  end
;;

(* this function raise Format822.ParseError *)
let input_raw_in ic =
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
    let l = input_raw_in ch in
    let _ = Input.close_ch ch in
    l
  with 
  |Input.File_empty -> (default_request,[])
  |Format822.ParseError (cl,field,errmsg) ->
      fatal "Filename %s\n %s\n %s : %s" file (String.concat "\n " cl) field errmsg
;;
