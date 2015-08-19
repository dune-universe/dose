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

class package ?(name=("Package",None)) ?(version=("Version",None)) ?(depends=("Depends",None))
    ?(conflicts=("Conflicts",None)) ?(provides=("Provides",None)) ?(recommends=("Recommends",None)) 
    ?(switch=("Switch",None)) ?(build_depends=("Build-Depends",None)) par = object
  
  inherit Pef.Packages.package ~name ~version ~depends ~conflicts ~provides ~recommends par

  val switch : string list =
    let f = Pef.Packages.parse_s ~opt:["all"] (Pef.Packages.parse_string_list ~rex:Pef.Packages.comma_regexp) in
    Pef.Packages.get_field_value f par switch

  val build_depends : Pef.Packages_types.vpkgformula =
    let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_vpkgformula in
    Pef.Packages.get_field_value f par build_depends

  method switch = switch
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
let parse_package_stanza ((switch,switches,profiles) as options) filter par =
  let p () =
    let pkg =
      let depends =
        let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_builddepsformula in
        ("Depends",Some (vpkgformula_filter options (f "Depends" par)))
      in
      let conflicts =
        let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_builddepslist in
        ("Conflicts",Some (vpkglist_filter options (f "Conflicts" par)))
      in
      let provides =
        let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_builddepslist in
        ("Provides",Some (vpkglist_filter options (f "Provides" par)))
      in
      let recommends =
        let f = Pef.Packages.parse_s ~opt:[] ~multi:true Pef.Packages.parse_builddepsformula in
        ("Recommends",Some (vpkgformula_filter options (f "Recommends" par)))
      in
      new package ~depends ~conflicts ~recommends ~provides par
    in
    if List.mem "all" pkg#switch then pkg else
    if List.exists (fun s -> List.mem s pkg#switch) (switch::switches) then pkg
    else
      raise (Pef.Packages.IgnorePackage (
        Printf.sprintf
        "None of the active switches [%s] are available [%s]" 
        (ExtString.String.join "," (switch::switches))
        (ExtString.String.join "," pkg#switch)
        )
      )
  in
  try
    if Option.is_none filter then Some (p ())
    else if (Option.get filter) par then Some (p ())
    else None
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

let parse_packages_in ?filter options fname ic =
  info "Parsing Packages file %s..." fname;
  try
    let stanza_parser = parse_package_stanza options filter in
    Format822.parse_from_ch (Pef.Packages.packages_parser fname stanza_parser []) ic
  with Pef.Packages.ParseError (field,errmsg) -> fatal "Filename %s\n %s : %s" fname field errmsg

module Set = struct
  let pkgcompare p1 p2 = compare (p1#name,p1#version) (p2#name,p2#version)
  include Set.Make(struct
    type t = package
    let compare (x:t) (y:t) = pkgcompare x y
  end)
end

(* XXX switches = empty ==> all switches are available ?? *)
let input_raw ?filter ?(switch="system") ?(switches=[]) ?(profiles=[]) =
  let module M = Format822.RawInput(Set) in
  M.input_raw (parse_packages_in ?filter (switch,switches,profiles))

let input_raw_ch ?filter ?(switch="system") ?(switches=[]) ?(profiles=[]) =
  let module M = Format822.RawInput(Set) in
  M.input_raw_ch (parse_packages_in ?filter (switch,switches,profiles))
