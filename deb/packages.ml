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

let parse_name = parse_package
let parse_vpkg = parse_constr
let parse_veqpkg = parse_constr
let parse_conj s = parse_vpkglist parse_vpkg s
let parse_cnf s = parse_vpkgformula parse_vpkg s
let parse_prov s = parse_veqpkglist parse_veqpkg s
let parse_essential = function
  |("Yes"|"yes") -> true
  |("No" | "no") -> false (* this one usually is not there *)
  |s -> fatal "Field essential has a wrong value : %s" s

let parse_packages_fields default_arch extras par =
  let extras = "status"::extras in
  let parse_s f field = f (single_line field (List.assoc field par)) in
  let parse_m f field = f (String.concat " " (List.assoc field par)) in
  let guard =
    match default_arch with
    |None -> fun _ -> true (* no filter *)
    |Some default_arch ->
        let default_arch = String.lowercase default_arch in
        fun arch -> (default_arch = arch || arch = "all")
  in
  let parse_e extras =
    List.filter_map (fun prop -> 
      let prop = String.lowercase prop in
      try Some (prop,single_line prop (List.assoc prop par))
      with Not_found -> None
    ) extras
  in
  let exec () = 
      {
        name = parse_s parse_name "package";
        version = parse_s parse_version "version";
        architecture = parse_s (fun x -> String.lowercase x) "architecture";
        essential = (try parse_s parse_essential "essential" with Not_found -> false);
        source = (try parse_s parse_source "source" with Not_found -> ("",None));
        depends = (try parse_m parse_cnf "depends" with Not_found -> []);
        pre_depends = (try parse_m parse_cnf "pre-depends" with Not_found -> []);
        recommends = (try parse_m parse_cnf "recommends" with Not_found -> []);
        suggests = (try parse_m parse_conj "suggests" with Not_found -> []);
        enhances = (try parse_m parse_conj "enhances" with Not_found -> []);
        conflicts = (try parse_m parse_conj "conflicts" with Not_found -> []);
        breaks = (try parse_m parse_conj "breaks" with Not_found -> []);
        replaces = (try parse_m parse_conj "replaces" with Not_found -> []);
        provides = (try parse_m parse_prov "provides" with Not_found -> []);
        priority = (try parse_s (fun x -> String.lowercase x) "priority" with Not_found -> "");
        extras = parse_e extras;
      }
  in
  (* this package doesn't either have version or name or architecture *)
  try if guard (parse_s (fun x -> x) "architecture") then Some(exec ()) else None 
  with Not_found -> begin
    let p = try parse_s (fun x -> x) "package" with Not_found -> "(MISSING NAME)" in
    let v = try parse_s (fun x -> x) "version" with Not_found -> "(MISSING VERSION)" in
    let a = try parse_s (fun x -> x) "architecture" with Not_found -> "(MISSING ARCH)" in
    warning "Ignoring broken package %s-%s.%s" p v a  ;
    None 
  end

(** parse a debian Packages file from the channel [ch] *)
let parse_packages_in ?(default_arch=None) ?(extras=[]) f ch =
  let parse_packages = Format822.parse_822_iter (parse_packages_fields default_arch extras) in
  parse_packages f (start_from_channel ch)

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

