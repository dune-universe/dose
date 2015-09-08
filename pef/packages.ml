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

(** Representation of a PEF stanza. *)

module Pcre = Re_pcre
open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

exception ParseError of string * string
exception IgnorePackage of string

(* here the _loc is taken from the the caller and not from the parser *)
let lexbuf_wrapper field type_parser (_loc,s) =
  try type_parser Packages_lexer.token_deb (Lexing.from_string s) with
  |Format822.Syntax_error (m) -> 
      let msg = Printf.sprintf "Field %s has a wrong value (%s): '%s'" field m s in
      raise (ParseError (field,msg))
  |Parsing.Parse_error -> 
      let msg = Printf.sprintf "Field %s has a wrong value: '%s'" field s in
      raise (ParseError (field,msg))

let parse_name field = lexbuf_wrapper field Packages_parser.pkgname_top
let parse_version field = lexbuf_wrapper field Packages_parser.version_top
let parse_vpkg field = lexbuf_wrapper field Packages_parser.vpkg_top
let parse_vpkglist field = lexbuf_wrapper field Packages_parser.vpkglist_top
let parse_vpkgformula field = lexbuf_wrapper field Packages_parser.vpkgformula_top
let parse_archlist field = lexbuf_wrapper field Packages_parser.archlist_top
let parse_builddepslist field = lexbuf_wrapper field Packages_parser.builddepslist_top
let parse_builddepsformula field = lexbuf_wrapper field Packages_parser.builddepsformula_top

(* assume n is lowercase *)
(* Using lists in this case is faster then using 
 * specialized Maps or Hashtables : tested ! *)
let rec assoc (n : string) = function
  |(k,v)::_ when k = n -> v
  |(k,_)::t -> assoc n t
  |[] -> raise Not_found

(* opt = None && err = None -> Not_found : this is for extras
 * opt = None && err = Some s -> ParseError s :
 * opt = Some s -> return s *)
let parse_s ?opt ?err ?(multi=false) f field par =
  try let (_loc,s) = (assoc field par) in f field (_loc,s)
  with Not_found ->
    if Option.is_none opt then
      if Option.is_none err then raise Not_found
      else begin
        raise (ParseError (field,(Option.get err)^" (no default declared) "))
      end
    else Option.get opt

let parse_string _ (_,s) = s
let parse_int _ (_,s) = int_of_string s
let parse_string_opt _ = function (_,"") -> None | (_,s) -> Some s

let blank_regexp = Pcre.regexp "[ \t]+" ;;
let comma_regexp = Pcre.regexp "[ \t]*,[ \t]*" ;;
let parse_string_list ?(rex=blank_regexp) _ (_,s) = Pcre.split ~rex s

(* parse and convert to a specific type *)
let parse_bool field = function
  |(_,("Yes"|"yes"|"True" |"true")) -> true
  |(_,("No" |"no" |"False"|"false")) -> false (* this one usually is not there *)
  |(_,s) -> raise (Format822.Type_error (field ^ " - wrong value : "^ s))
;;

let parse_bool_s field v = string_of_bool (parse_bool field v) ;;
let parse_int_s field (_,s) = string_of_int (int_of_string s) ;;

(* parse extra fields parse_f returns a string *)
let parse_e extras par =
  List.filter_map (fun (field, p) ->
    try begin
      match p with
      |None -> Some(field,parse_s parse_string field par)
      |Some parse_f -> Some (field,parse_f par)
    end with Not_found -> None
  ) extras

let get_field_value f par (field,value) =
  let res = 
    if Option.is_none value then f field par
    else Option.get value
  in (field,res)

(** strip down version of the debian package format *)
class package 
  ?(name=("Package",None)) ?(version=("Version",None)) 
  ?(installed=("Installed",Some false)) ?(depends=("Depends",None))
  ?(conflicts=("Conflicts",None)) ?(provides=("Provides",None)) 
  ?(recommends=("Recommends",None)) ?(extras=([],None)) par = object

  val name : (string * Packages_types.name) =
    let f = parse_s ~err:"(MISSING NAME)" parse_name in
    get_field_value f par name

  val version : (string * Packages_types.version) =
    let f = parse_s ~err:"(MISSING VERSION)" parse_version in
    get_field_value f par version

  val installed : (string * Packages_types.installed) =
    let f = parse_s ~opt:false parse_bool in
    get_field_value f par installed

  val depends : (string * Packages_types.vpkgformula) =
    let f = parse_s ~opt:[] ~multi:true parse_vpkgformula in
    get_field_value f par depends

  val conflicts : (string * Packages_types.vpkglist) =
    let f = parse_s ~opt:[] ~multi:true parse_vpkglist in
    get_field_value f par conflicts

  val provides : (string * Packages_types.vpkglist) =
    let f = parse_s ~opt:[] ~multi:true parse_vpkglist in
    get_field_value f par provides

  val recommends : (string * Packages_types.vpkgformula) =
    let f = parse_s ~opt:[] ~multi:true parse_vpkgformula in
    get_field_value f par recommends

  val extras : (string * string) list =
    match extras with
    |([],None) -> []
    |(extras,None) -> parse_e extras par
    |([],Some l) -> l
    |(extras,Some l) -> l@(parse_e extras par)
  
  method name = snd name
  method version = snd version
  method installed = snd installed
  method depends = snd depends
  method conflicts = snd conflicts
  method provides = snd provides
  method recommends = snd recommends
  method extras = extras

  method add_extra k v = {< extras = (k,v)::extras >}
  method get_extra k = assoc k extras
  method set_extras v = {< extras = v >}

  method set_installed v = {< installed = (fst installed,v) >}

  method pp oc =
    Printer.pp_string oc name;
    Printer.pp_string oc version;
    Printer.pp_vpkglist oc provides;
    Printer.pp_vpkgformula oc depends;
    Printer.pp_vpkglist oc conflicts;
    Printer.pp_vpkgformula oc recommends;
    Printf.fprintf oc "\n"

end

let parse_package_stanza filter extras par =
  let p () = new package ~extras:(extras,None) par in
  if Option.is_none filter then Some (p ())
  else if (Option.get filter) par then Some(p ())
  else None

(* parse the entire file while filtering out unwanted stanzas *)
let rec packages_parser fname stanza_parser acc p =
  let filename = ("Filename",(Format822.dummy_loc,Filename.basename fname)) in
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> acc
  |Some stanza -> begin
    match stanza_parser (filename::stanza) with
    |None -> packages_parser fname stanza_parser acc p
    |Some st -> packages_parser fname stanza_parser (st::acc) p
  end

let parse_packages_in ?filter ?(extras=[]) fname ic =
  info "Parsing 822 file %s..." fname;
  try
    let stanza_parser = parse_package_stanza filter extras in
    Format822.parse_from_ch (packages_parser fname stanza_parser []) ic
  with ParseError (field,errmsg) -> fatal "Filename %s\n %s : %s" fname field errmsg

(**/**)
module Set = struct
  let pkgcompare p1 p2 = compare (p1#name,p1#version) (p2#name,p2#version)
  include Set.Make(struct 
    type t = package
    let compare (x:t) (y:t) = pkgcompare x y
  end)
end
(**/**)

let input_raw ?(extras=[]) = 
  let module M = Format822.RawInput(Set) in
  M.input_raw (parse_packages_in ~extras)

let input_raw_ch ?(extras=[]) = 
  let module M = Format822.RawInput(Set) in
  M.input_raw_ch (parse_packages_in ~extras)
