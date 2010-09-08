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

let debug fmt = Util.make_debug "Distcheck" fmt
let info fmt = Util.make_info "Distcheck" fmt
let warning fmt = Util.make_warning "Distcheck" fmt

let filter_packages pred =
  Cudf.fold_packages
    (fun acc p -> if pred p then p::acc else acc)
    []
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
  let (universe,from_cudf,_) =
    Boilerplate.load_universe ~default_arch posargs in

  (* assure that binary and source packages appear only once *) 
  let versions = Hashtbl.create (Cudf.universe_size universe)
  and src_versions = Hashtbl.create (Cudf.universe_size universe)
  and cruft_sources = ref []
  and cruft_binaries = ref []
  in begin
    Cudf.iter_packages
      (fun p ->
	 let name = p.Cudf.package
	 and version = p.Cudf.version
	 and src_name = List.assoc "source" p.Cudf.pkg_extra
	 and src_version = List.assoc "sourceversion" p.Cudf.pkg_extra
	 in begin
	     try
	       let oldversion = Hashtbl.find versions name
	       in 
		 if oldversion < version 
		 then begin
		   cruft_binaries := (name,oldversion)::!cruft_binaries;
		   Hashtbl.add versions name version
		 end
		 else if version < oldversion
		 then cruft_binaries := (name,version)::!cruft_binaries
	     with
		 Not_found -> Hashtbl.add versions name version
	   end;
	   begin
	     try
	       let oldsrc_version = Hashtbl.find src_versions src_name
	       in 
		 if oldsrc_version < src_version (** TODO: debian version comparison!! *)
		 then begin
		   cruft_sources := (src_name,src_version)::!cruft_sources;
		   Hashtbl.add src_versions src_name src_version
		 end
		 else  if src_version < oldsrc_version
		 then cruft_sources := (src_name,src_version)::!cruft_sources
	     with
		 Not_found -> Hashtbl.add src_versions src_name src_version	   
	   end)
      universe;
      let filtered_universe =
	filter_packages
	  (fun p ->
	     let name = p.Cudf.package
	     and version = p.Cudf.version
	     and deb_version =
	       match List.assoc "number" p.Cudf.pkg_extra with
		   `String s -> s
		 | _ -> failwith "debian version of wrong type"
	     and src_name = List.assoc "source" p.Cudf.pkg_extra
	     and src_version = List.assoc "sourceversion" p.Cudf.pkg_extra
	     in
	       if List.mem (name,version) !cruft_binaries
	       then begin
		 print_string ("Package ("^name^","^deb_version^") ignored.\n");
		 false
	       end
	       else if List.mem (src_name,src_version) !cruft_sources
	       then begin
		 print_string ("Package ("^name^","^deb_version^") ignored since cruft source.\n");
		 false
	       end
	       else true
	  )
	  universe
      in ()
    end;

    let from_cudf p = from_cudf (p.Cudf.package,p.Cudf.version) in 
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

