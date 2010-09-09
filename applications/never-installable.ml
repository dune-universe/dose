(****************************************************************************)
(*  Copyright (C) 2010 Ralf Treinen <treinen@pps.jussieu.fr>                *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU General Public License as published by     *)
(* the Free Software Foundation, either version 3 of the License, or        *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU General Public License for more details.                             *)
(*                                                                          *)
(* You should have received a copy of the GNU General Public License        *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(****************************************************************************)


open Debian
open Common
open Diagnostic

module Options = struct
  open OptParse

  let verbose = StdOpt.incr_option ()
  let explain = StdOpt.store_true ()
  let architecture = StdOpt.str_option ()

  let description = "Report outdated packages in a package list"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~long_name:"verbose" ~help:"Print additional information" verbose;
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'a' ~long_name:"architecture" ~help:"Set the default architecture" architecture;
end

let debug fmt = Util.make_debug "" fmt
let info fmt = Util.make_info "" fmt
let warning fmt = Util.make_warning "" fmt

let filter_packages pred =
  Cudf.fold_packages
    (fun acc p -> if pred p then p::acc else acc)
    []
;;

let sourcename_of_package p = 
  try
    match List.assoc "source" p.Cudf.pkg_extra with
	`String s -> s
      | _ -> failwith "CUDF: source field of wrong type"
  with
      Not_found -> failwith "CUDF: source field missing"
;;

let sourceversion_of_package p = 
  try
    match List.assoc "sourceversion" p.Cudf.pkg_extra with
	`String s -> s
      | _ -> failwith "CUDF: source version field of wrong type"
  with
      Not_found -> failwith "CUDF: source version missing"
;;

let debversion_of_package p = 
  try
    match List.assoc "number" p.Cudf.pkg_extra with
	`String s -> s
      | _ -> failwith "debian version of wrong type"
  with
      Not_found -> failwith "CUDF: debian version missing"
;;

let chop_binnmu s =
  (* chops a possible bin-NMU suffix from a debian version string *)
  try
    Str.string_before s
      (Str.search_backward (Str.regexp "\\+b[0-9]+$") s ((String.length s)-1))
  with
      Not_found -> s
;;


(* (purge_universe universe) returns a subset of universe obtained by retaing only the highest *)
(* version of each binary package, and only the highest version of each source package.        *)
(* Prints a warning for each surpressed package.                                               *)
let purge_universe universe =

  let versions = Hashtbl.create (Cudf.universe_size universe)
    (* associates to a binary package name the pair (latest cudf version, latest debian version) *)
  and src_versions = Hashtbl.create (Cudf.universe_size universe)
    (* associates to a source package name the latest debian version *)
  and cruft_sources = ref []
    (* association list, associates to an obsolete (package name, package cudf version) the *)
    (* newer debian version *)
  and cruft_binaries = ref []
    (* association list, associates to an obsolete (source name, source debian version) the *)
    (* newer debian version. *)
  in

  (* identify cruft *)
  Cudf.iter_packages
    (fun p ->
      let name = p.Cudf.package
      and version = p.Cudf.version
      and deb_version = debversion_of_package p
      and src_name = sourcename_of_package p
      and src_version = sourceversion_of_package p
      in begin
	try
	  let (oldversion,olddeb_version) = Hashtbl.find versions name
	  in 
	  if oldversion < version 
	  then begin
	    cruft_binaries := ((name,oldversion),deb_version)::!cruft_binaries;
	    Hashtbl.add versions name (version,deb_version)
	  end
	  else if version < oldversion
	  then cruft_binaries := ((name,version),olddeb_version)::!cruft_binaries
	with
	    Not_found -> Hashtbl.add versions name (version,deb_version)
      end;
      begin
	try
	  let oldsrc_version = Hashtbl.find src_versions src_name
	  in 
	  let c = Version.compare oldsrc_version src_version
	  in if c<0 then begin
	    cruft_sources := ((src_name,oldsrc_version),src_version)::!cruft_sources;
	    Hashtbl.add src_versions src_name src_version
	  end else if c>0 then begin
	    cruft_sources := ((src_name,src_version),oldsrc_version)::!cruft_sources
	  end
	with
	    Not_found -> Hashtbl.add src_versions src_name src_version	   
      end)
    universe;

  (* filter out cruft *)
  Cudf.load_universe
    (filter_packages
       (fun p ->
	 let name = p.Cudf.package
	 and version = p.Cudf.version
	 and deb_version = debversion_of_package p
	 and src_name = sourcename_of_package p
	 and src_version = sourceversion_of_package p
	 in
	 try
	   let newer_version = List.assoc (name,version) !cruft_binaries
	   in begin
	       warning
		 "Package %s(%s) ignored: there is a newer version %s"
		 name deb_version newer_version;
	       false
	   end
	 with Not_found->
	   try
	     let newer_src_version =
	       List.assoc (src_name,src_version) !cruft_sources
	     in begin
		 warning
		   ("Package %s(%s) ignored: stems from source %s(%s) but this source has newer version %s")
		   name deb_version src_name src_version newer_src_version;
	       false
	     end
	   with Not_found -> true
       )
       universe
    )
;;

let hash_add_tolist table key value =
  try
    let old = Hashtbl.find table key
    in old := (value::!old)
  with
      Not_found -> Hashtbl.add table key (ref [value])
;;

let synchronization_table universe =
(* returns a hash table that associates names of source packages to names *)
(* of binary packages. The table associates s to b if s is the source     *)
(* package of some version of b, and if all binary packages coming from   *)
(* source s have the same version number up to binary NMU.                *)

  let packages_of_source = Hashtbl.create (Cudf.universe_size universe)
  in 
    Cudf.iter_packages
      (fun p -> 
	 let name = p.Cudf.package
	 and deb_version = debversion_of_package p
	 and src_name = sourcename_of_package p
	 in
	   hash_add_tolist packages_of_source src_name (name,deb_version)
      )
      universe;
    let sync_table = Hashtbl.create (Cudf.universe_size universe)
    in 
      Hashtbl.iter 
	(fun src bins_ref ->
	   let bins = !bins_ref
	   in
	     if 1=List.length
	       (ExtLib.List.unique (List.map chop_binnmu (List.map snd bins)))
	     then
	       List.iter
		 (fun (binp,_binv) -> Hashtbl.add sync_table binp src)
		 bins
	     else begin
	       warning "Binary packages of source %s not synchronized" src;
	       List.iter
		 (fun (name,version) -> warning "  %s(%s)" name version)
		 bins;
	     end)
	packages_of_source;
      sync_table
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs =
    let args = OptParse.OptParser.parse_argv Options.options in
    match args with
    |[] -> ["deb://-"]
    |l -> List.map ((^) "deb://") l
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  let default_arch = OptParse.Opt.opt Options.architecture in
  let (complete_universe,from_cudf,_) =
    Boilerplate.load_universe ~default_arch posargs in
  let from_cudf p = from_cudf (p.Cudf.package,p.Cudf.version)
  and universe = purge_universe complete_universe in
  let sync_table = synchronization_table universe in
      info "Solving..." ;
      let timer = Util.Timer.create "Solver" in
	Util.Timer.start timer;
	let explain = OptParse.Opt.get Options.explain in
	let fmt = Format.std_formatter in
	  Format.fprintf fmt "@[<v 1>report:@,";
	  let callback = Diagnostic.fprintf ~pp:from_cudf ~failure:true ~success:false ~explain fmt in
	  let i = Depsolver.univcheck ~callback universe 
	  in
	    ignore(Util.Timer.stop timer ());
	    Format.fprintf fmt "@]@.";
	    Format.fprintf fmt "total-packages: %d\n" (Cudf.universe_size universe);
	    Format.fprintf fmt "broken-packages: %d\n" i;
	    if OptParse.Opt.is_set Options.architecture then
	      Format.fprintf fmt "architecture: %s\n" (OptParse.Opt.get Options.architecture)
;;

main () ;;

