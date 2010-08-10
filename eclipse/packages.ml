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
open Debian.Format822

(** strip down version of the debian package format *)
type package = {
  name : name ;
  version : version;
  depends : vpkg list list;
  conflicts : vpkg list;
  provides : veqpkg list;
  recommends : vpkg list list;
  suggests : vpkg list;
  extras : (string * string) list;
}

let default_package = {
  name = "";
  version = "";
  depends = [];
  conflicts = [];
  provides = [];
  recommends = [];
  suggests = [];
  extras = [];
}

let parse_name s = s
let parse_version s = Version.parse_version s
let parse_constr s =
  let s = start_token_stream s in
  parse_constr_aux ~check:false true s

let parse_vpkg = parse_constr
let parse_veqpkg = parse_constr
let parse_conj s = parse_vpkglist parse_vpkg s
let parse_cnf s = parse_vpkgformula parse_vpkg s
let parse_prov s = parse_veqpkglist parse_veqpkg s

let parse_packages_fields extras par =
  let extras = "status"::extras in
  let parse_s f field = f (single_line field (List.assoc field par)) in
  let parse_m f field = f (String.concat " " (List.assoc field par)) in
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
        depends = (try parse_m parse_cnf "depends" with Not_found -> []);
        conflicts = (try parse_m parse_conj "conflicts" with Not_found -> []);
        provides = (try parse_m parse_prov "provides" with Not_found -> []);
        recommends = (try parse_m parse_cnf "recommends" with Not_found -> []);
        suggests = (try parse_m parse_conj "suggests" with Not_found -> []);
        extras = parse_e extras;
      }
  in
  (* this package doesn't either have version or name or architecture *)
  try Some(exec ()) with Not_found -> begin
    let p = try parse_s (fun x -> x) "package" with Not_found -> "" in
    let v = try parse_s (fun x -> x) "version" with Not_found -> "" in
    Util.print_warning "Broken Package %s-%s" p v ;
    None 
  end

(** parse a debian Packages file from the channel [ch] *)
let parse_packages_in ?(extras=[]) f ch =
  let parse_packages = Debian.Format822.parse_822_iter (parse_packages_fields extras) in
  parse_packages f (start_from_channel ch)

(**/**)
module Set = struct
  let pkgcompare p1 p2 = compare (p1.name,p1.version) (p2.name,p2.version)
  include Set.Make(struct 
    type t = package
    let compare = pkgcompare
  end)
end
(**/**)

(** input_raw [file] : parse a debian Packages file from [file] *)
let input_raw ?(extras=[]) = 
  let module M = Debian.Format822.RawInput(Set) in
  M.input_raw (parse_packages_in ~extras)

(** input_raw_ch ch : parse a debian Packages file from channel [ch] *)
let input_raw_ch ?(extras=[]) = 
  let module M = Debian.Format822.RawInput(Set) in
  M.input_raw_ch (parse_packages_in ~extras)
