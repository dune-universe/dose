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
open Format822

let debug fmt = Util.make_debug "Debian.Packages" fmt
let info fmt = Util.make_info "Debian.Packages" fmt
let warning fmt = Util.make_warning "Debian.Packages" fmt
let fatal fmt = Util.make_fatal "Debian.Packages" fmt

(** debian package format *)
type package = {
  name : name ;
  version : version;
  architecture : string;
  essential : bool;
  source : (name * version option) ;
  depends : vpkg list list;
  pre_depends : vpkg list list;
  recommends : vpkg list list;
  suggests : vpkg list;
  enhances : vpkg list;
  conflicts : vpkg list;
  breaks : vpkg list;
  replaces : vpkg list;
  provides : veqpkg list;
  extras : (string * string) list;
  priority : string;
}

let default_package = {
  name = "";
  version = "";
  architecture = "";
  essential = false;
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
  priority = "";
}

exception ParseError of string * string
exception IgnorePackage of string

let parse_name _ s = Format822.parse_package s
let parse_version _ s = Format822.parse_version s
let parse_source _ s = Format822.parse_source s
let parse_vpkg _ s = Format822.parse_constr s
let parse_veqpkg _ s = Format822.parse_constr s
let parse_conj _ s = Format822.parse_vpkglist Format822.parse_constr s
let parse_cnf _ s = Format822.parse_vpkgformula Format822.parse_constr s
let parse_prov _ s = Format822.parse_veqpkglist Format822.parse_constr s
let parse_bool field = function
  |("Yes"|"yes") -> true
  |("No" | "no") -> false (* this one usually is not there *)
  |s -> fatal "Field %s has a wrong value : %s" field s
let parse_string _ s = String.lowercase s
let parse_int _ s = int_of_string s

let parse_architecture default_arch _ s = 
  match default_arch with
  |None -> String.lowercase s
  |Some default_arch ->
      let default_arch = String.lowercase default_arch in
      let arch = String.lowercase s in
      if (default_arch = arch || arch = "all") then arch else
        raise (IgnorePackage (
          Printf.sprintf 
          "architecture: %s is different from %s (default) or 'all'" 
          arch default_arch
          )
        )
;;

let parse_packages_fields default_arch extras par =
  let parse_s ?opt ?(err="") ?(multi=false) f field =
    let delayed_f () =
      let line =
        let s = List.assoc field par in
        if multi then String.concat " " s
        else Format822.single_line field s
      in
      f field line
    in
    if Option.is_none opt then
      try delayed_f ()
      with Not_found -> raise (ParseError (field,err))
    else
      try delayed_f ()
      with Not_found -> Option.get opt
  in
  let parse_e extras =
    List.filter_map (fun prop -> 
      let prop = String.lowercase prop in
      try Some (prop,single_line prop (List.assoc prop par))
      with Not_found -> None
    ) extras
  in
  try
    Some {
        name = parse_s ~err:"(MISSING NAME)" parse_name "package";
        version = parse_s ~err:"(MISSING VERSION)" parse_version "version";
        architecture = parse_s ~err:"(MISSING ARCH)" (parse_architecture default_arch) "architecture";
        source = parse_s ~opt:("",None) parse_source "source";

        essential = parse_s ~opt:false parse_bool "essential";
        priority = parse_s ~opt:"" parse_string "priority";

        depends = parse_s ~opt:[] ~multi:true parse_cnf "depends";
        pre_depends = parse_s ~opt:[] ~multi:true parse_cnf "pre-depends";
        recommends = parse_s ~opt:[] ~multi:true parse_cnf "recommends";
        suggests = parse_s ~opt:[] ~multi:true parse_conj "suggests";
        enhances = parse_s ~opt:[] ~multi:true parse_conj "enhances";
        conflicts = parse_s ~opt:[] ~multi:true parse_conj "conflicts";
        breaks = parse_s ~opt:[] ~multi:true parse_conj "breaks";
        replaces = parse_s ~opt:[] ~multi:true parse_conj "replaces";
        provides = parse_s ~opt:[] ~multi:true parse_conj "provides";
        extras = parse_e extras;
    }
  with 
    |ParseError (f,s) -> begin
      List.iter (fun (k,v) -> Printf.eprintf "%s: %s\n%!" k (String.concat " " v)) par;
      warning "Parse Error in field %s : %s" f s;
      None
    end
    |IgnorePackage s -> begin
      List.iter (fun (k,v) -> Printf.eprintf "%s: %s\n%!" k (String.concat " " v)) par;
      warning "Package Ignored : %s" s;
      None
    end
;;

(** parse a debian Packages file from the channel [ch] *)
let parse_packages_in ?(default_arch=None) ?(extras=[]) f ch =
  let parse_packages = 
    Format822.parse_822_iter (parse_packages_fields default_arch extras) 
  in
  parse_packages f (Format822.start_from_channel ch)

(**/**)
let id p = (p.name,p.version,p.architecture)
let (>%) p1 p2 = Pervasives.compare (id p1) (id p2)
module Set = struct
  include Set.Make(struct 
    type t = package
    let compare = (>%)
  end)
end
(**/**)

let merge status packages =
  let merge_aux p1 p2 =
    if (p1 >% p2) = 0 then begin
      {p1 with
        essential = p1.essential || p2.essential;
        extras = List.unique (p1.extras @ p2.extras)
      }
    end else assert false
  in
  let h = Hashtbl.create (List.length status) in
  List.iter (fun p ->
    try
      match String.nsplit (List.assoc "status" p.extras) " " with
      |[_;_;"installed"] -> Hashtbl.add h (id p) p
      |_ -> ()
    with Not_found -> ()
  ) status
  ;
  let ps = 
    List.fold_left (fun acc p ->
      try Set.add (merge_aux p (Hashtbl.find h (id p))) acc
      with Not_found -> Set.add p acc
    ) Set.empty (status @ packages)
  in
  Set.elements ps

(** input_raw [file] : parse a debian Packages file from [file] *)
let input_raw ?(default_arch=None) ?(extras=[]) = 
  let module M = Format822.RawInput(Set) in
  M.input_raw (parse_packages_in ~default_arch ~extras)

(** input_raw_ch ch : parse a debian Packages file from channel [ch] *)
let input_raw_ch ?(default_arch=None) ?(extras=[]) = 
  let module M = Format822.RawInput(Set) in
  M.input_raw_ch (parse_packages_in ~default_arch ~extras)
