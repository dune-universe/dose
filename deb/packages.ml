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

(** Representation of a debian package description item. *)

open ExtLib
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

(** debian package format *)
type package = {
  name : Format822.name ;
  version : Format822.version;
  architecture : Format822.architecture ;
  multiarch : Format822.multiarch ;
  essential : bool;
  build_essential : bool;
  priority : string;
  source : (Format822.name * Format822.version option) ;
  depends : Format822.vpkgformula ;
  pre_depends : Format822.vpkgformula ;
  recommends : Format822.vpkgformula ;
  suggests : Format822.vpkgformula;
  enhances : Format822.vpkgformula;
  conflicts : Format822.vpkglist;
  breaks : Format822.vpkglist;
  replaces : Format822.vpkglist;
  provides : Format822.vpkglist;
  extras : (string * string) list;
}

let default_package = {
  name = "";
  version = "";
  architecture = "";
  multiarch = `None;
  essential = false;
  build_essential = false;
  priority = "";
  depends = [];
  source = ("",None);
  pre_depends = [];
  recommends = [];
  suggests = [];
  enhances = [];
  conflicts = [];
  breaks = [];
  replaces = [];
  provides = [];
  extras = [];
}

(* here the _loc is taken from the the caller and not from the parser *)
let lexbuf_wrapper type_parser (_loc,s) =
  try type_parser Packages_lexer.token_deb (Lexing.from_string s) 
  with Format822.Syntax_error (_msg, _) ->
   raise (Format822.Syntax_error (s, _loc))

let parse_name = lexbuf_wrapper Packages_parser.pkgname_top
let parse_version = lexbuf_wrapper Packages_parser.version_top
let parse_multiarch = lexbuf_wrapper Packages_parser.multiarch_top
let parse_source = lexbuf_wrapper Packages_parser.source_top
let parse_vpkg = lexbuf_wrapper Packages_parser.vpkg_top
let parse_vpkglist = lexbuf_wrapper Packages_parser.vpkglist_top
let parse_vpkgformula = lexbuf_wrapper Packages_parser.vpkgformula_top
let parse_binarylist = lexbuf_wrapper Packages_parser.vpkglist_top

(**************************************)

(* assume n is lowercase *)
let rec assoc (n : string) = function
  |(k,v)::_ when (String.lowercase k) = n -> v
  |(k,_)::t -> assoc n t
  |[] -> raise Not_found
;;

exception ParseError of string * string
exception IgnorePackage of string

(* opt = None && err = None -> Not_found : this is for extras
 * opt = None && err = Some s -> ParseError s :
 * opt = Some s -> return s *)
let parse_s ?opt ?err ?(multi=false) f field par =
  let field = String.lowercase field in
  try let (_loc,s) = (assoc field par) in f (_loc,s) 
  with Not_found ->
    if Option.is_none opt then
      if Option.is_none err then raise Not_found
      else begin
        let (_,((startpos,endpos),_)) = List.hd par in
        let s = 
          Printf.sprintf "%s : %s--%s" 
          (Format822.pp_posfname startpos)
          (Format822.pp_lpos startpos) 
          (Format822.pp_lpos endpos)
        in
        raise (ParseError (field,(Option.get err)^" (no default declared) " ^ s))
      end
    else Option.get opt
;;

let parse_string (_,s) = s
let parse_int (_,s) = int_of_string s

(* parse extra fields parse_f returns a string *)
let parse_e extras par =
  List.filter_map (fun (field, p) ->
    try begin 
      match p with 
      |None -> Some(field,parse_s parse_string field par)
      |Some parse_f -> Some (field,parse_f par)
    end with Not_found -> None
  ) extras
;;

(* parse and convert to a specific type *)
let parse_bool = function
  |(_,("Yes"|"yes"|"True" |"true")) -> true
  |(_,("No" |"no" |"False"|"false")) -> false (* this one usually is not there *)
  |(_,s) -> assert false (*raise (Format822.Type_error ("wrong value : "^ s))*)

(* this function make sure that the "all" arch is always considered *)
let parse_architecture archs (_,arch) =
  match archs with
  |[] -> arch
  |l -> 
      if List.mem arch ("all"::archs) then arch else
        raise (IgnorePackage (
          Printf.sprintf
          "architecture: %s is not included in %s"
          arch (ExtString.String.join "," ("all"::archs))
          )
        )
;;

let parse_package_stanza filter archs extras par =
  let parse_arch = parse_architecture archs in
  let p () = {
      name = parse_s ~err:"(MISSING NAME)" parse_name "Package" par;
      version = parse_s ~err:"(MISSING VERSION)" parse_version "Version" par;
      architecture = parse_s ~err:"(MISSING ARCH)" parse_arch "Architecture" par;
      multiarch = parse_s ~opt:`None parse_multiarch "Multi-Arch" par;
      source = parse_s ~opt:("",None) parse_source "Source" par;

      essential = parse_s ~opt:false parse_bool "Essential" par;
      build_essential = parse_s ~opt:false parse_bool "Build-Essential" par;
      priority = parse_s ~opt:"" parse_string "Priority" par;

      depends = parse_s ~opt:[] ~multi:true parse_vpkgformula "Depends" par;
      pre_depends = parse_s ~opt:[] ~multi:true parse_vpkgformula "Pre-Depends" par;
      recommends = parse_s ~opt:[] ~multi:true parse_vpkgformula "Recommends" par;
      suggests = parse_s ~opt:[] ~multi:true parse_vpkgformula "Suggests" par;
      enhances = parse_s ~opt:[] ~multi:true parse_vpkgformula "Enhances" par;
      conflicts = parse_s ~opt:[] ~multi:true parse_vpkglist "Conflicts" par;
      breaks = parse_s ~opt:[] ~multi:true parse_vpkglist "Breaks" par;
      replaces = parse_s ~opt:[] ~multi:true parse_vpkglist "Replaces" par;
      provides = parse_s ~opt:[] ~multi:true parse_vpkglist "Provides" par;
      extras = ("Type","bin") :: (parse_e extras par);
  }
  in
  try
    if Option.is_none filter then Some (p ())
    else if (Option.get filter) par then Some(p ()) 
    else None
  with 
  |IgnorePackage s -> begin
      let n = parse_s ~opt:"?" parse_name "Package" par in
      let v = parse_s ~opt:"?" parse_version "Version" par in
      let a = parse_s ~opt:"?" parse_version "Architecture" par in
      warning "Ignoring Package (%s,%s,%s) : %s" n v a s; 
      None
    end
  |ParseError (f,s) -> begin
      let n = parse_s ~opt:"?" parse_name "Package" par in
      let v = parse_s ~opt:"?" parse_version "Version" par in
      let a = parse_s ~opt:"?" parse_version "Architecture" par in
      let err = Printf.sprintf "Parser Error in Package (%s,%s,%s) : %s" n v a s in
      raise ( ParseError (f,err) )
  end
;;

let status_filter par =
  try
    let (_,s) = (assoc "status" par) in
    match String.nsplit s " " with
    |[_;_;"installed"] -> true
    |_ -> false
  with Not_found -> false

let arch_filter archlist par =
  try
    let (_,s) = (assoc "architecture" par) in
    List.mem s archlist
  with Not_found -> false

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

let parse_packages_in ?filter ?(archs=[]) ?(extras=[]) fname ic =
  info "Parsing Packages file %s..." fname;
  try
    let stanza_parser = parse_package_stanza filter archs extras in
    Format822.parse_from_ch (packages_parser fname stanza_parser []) ic
  with ParseError (field,errmsg) -> fatal "Filename %s\n %s : %s" fname field errmsg

(**/**)
let id p = (p.name,p.version,p.architecture)
let (>%) p1 p2 = Pervasives.compare (id p1) (id p2)
module Set = struct
  include Set.Make(struct
    type t = package
    let compare x y =
      let c = x >% y in 
      if c = 0 then 
        debug "the input contains two packages with the same name, version and architecture (%s,%s,%s). Only the latter will be considered."
        x.name x.version x.architecture;
      c
  end)
end
(**/**)

let merge status packages =
  info "Merging status file";
  let merge_aux p1 p2 =
    if (p1 >% p2) = 0 then begin
      {p1 with
        essential = p1.essential || p2.essential;
        extras = List.unique (p1.extras @ p2.extras)
      }
    end else fatal "Something went wrong while merging status+packages"
  in
  let h = Hashtbl.create (List.length status) in
  List.iter (fun p -> Hashtbl.add h (id p) p) status ;
  let ps =
    List.fold_left (fun acc p ->
      try Set.add (merge_aux p (Hashtbl.find h (id p))) acc
      with Not_found -> Set.add p acc
    ) Set.empty (status @ packages)
  in
  Set.elements ps 

let is_installed pkg =
  try match String.nsplit (assoc "status" pkg.extras) " " with
    |[_;_;"installed"] -> true
    | _ -> false
  with Not_found -> false

let is_on_hold pkg =
  try match String.nsplit (assoc "status" pkg.extras) " " with
    |["hold";_;_] -> true
    | _ -> false
  with Not_found -> false

let default_extras = [
  ("Status", None);
  ("Size", None);
  ("Installed-Size", None);
  ("Filename", None);
]

(** input_raw [file] : parse a debian Packages file from [file]
    [~archs] determines which which architectures should be considered while
    parsing the Packages file. if ~arch is [] then all archs are cosidered 
*)
let input_raw ?filter ?(archs=[]) ?(extras=[]) =
  let module M = Format822.RawInput(Set) in
  let extras = default_extras @ extras in
  M.input_raw (parse_packages_in ?filter ~archs ~extras)
;;

(** input_raw_ch ch : parse a debian Packages file from channel [ch] *)
let input_raw_ch ?filter ?(archs=[]) ?(extras=[]) =
  let module M = Format822.RawInput(Set) in
  let extras = default_extras @ extras in
  M.input_raw_ch (parse_packages_in ?filter ~archs ~extras)
