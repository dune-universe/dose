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

(********************************************************************************************************)

(* (purge_universe universe) returns a subset of universe obtained by retaing only the highest *)
(* version of each binary package, and only the highest version of each source package.        *)
(* Prints a warning for each surpressed package.                                               *)
let purge_universe universe =

  let current_versions = Hashtbl.create (Cudf.universe_size universe)
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
	  let (oldversion,olddeb_version) = Hashtbl.find current_versions name
	  in 
	  if oldversion < version 
	  then begin
	    cruft_binaries := ((name,oldversion),deb_version)::!cruft_binaries;
	    Hashtbl.add current_versions name (version,deb_version)
	  end
	  else if version < oldversion
	  then cruft_binaries := ((name,version),olddeb_version)::!cruft_binaries
	with
	    Not_found -> Hashtbl.add current_versions name (version,deb_version)
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
	       warning "%s(%s) dropped:" name deb_version;
	       warning "  %s is newer." newer_version;
	       false
	   end
	 with Not_found->
	   try
	     let newer_src_version =
	       List.assoc (src_name,src_version) !cruft_sources
	     in begin
		 warning "%s(%s) dropped:" name deb_version;
		 warning "  stems from source %s(%s)" src_name src_version;
		 warning "  but %s is newer." newer_src_version;
		 false
	       end
	   with Not_found -> true
       )
       universe
    )
;;

(**********************************************************************************************************)

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

(****************************************************************************)
(* This section is largely copied from strongpred.ml !! *)

(* collect all possible versions *)
let add_to_versions_table table =
  let add name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  in
  let conj_iter =
    List.iter (fun (name,sel) ->
      match sel with
      |None -> ()
      |Some(_,version) -> add name version
    )
  in
  let cnf_iter =
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        match sel with
        |None -> ()
        |Some(_,version) -> add name version
      ) disjunction
    )
  in
  fun pkg ->
    add pkg.Cudf.package pkg.Cudf.version;
    conj_iter pkg.Cudf.conflicts ;
    conj_iter (pkg.Cudf.provides :> Cudf_types.vpkglist) ;
    cnf_iter pkg.Cudf.depends
;;

(* build a mapping between package names and a maps old version -> new version *)
let build_table universe =
  let version_table = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (add_to_versions_table version_table) universe;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Hashtbl.iter
    (fun k {contents=l} ->
       let c = ref 1 in
       let hv = Hashtbl.create (List.length l) in
	 List.iter
	   (fun n ->
	      (* Printf.eprintf "%s : %d -> %d\n" k n (2 * !c); *)
	      Hashtbl.add hv n (2 * !c);
	      c := !c + 1
	   )
	   (ExtLib.List.sort (ExtLib.List.unique l));
	 Hashtbl.add h k hv
    )
    version_table;
  h
;;

(* map the old universe to a pair of table {packages->l}, where l is  *)
(* is the list of all constraints in which the package is mentionend, *)
(* and package list, such that all package numbers are spaced out.    *)
let renumber universe = 
  let add table name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  in
  let h = build_table universe in
  let rh = Hashtbl.create (Cudf.universe_size universe) in
  let conj_map hv =
    List.map (fun (name,sel) ->
      match sel with
      |None -> (name,sel)
      |Some(c,v) -> begin
        let hv = Hashtbl.find h name in
        add rh name ((c :> Cudf_types.relop),Hashtbl.find hv v);
        (name,Some(c,Hashtbl.find hv v))
      end
    )
  in
  let cnf_map h =
    List.map (fun disjunction ->
      List.map (fun (name,sel) ->
        match sel with
        |None -> (name,sel)
        |Some(c,v) -> begin
          (* Printf.eprintf "->>>>>>>> %s %d\n" name v; *)
          let hv = Hashtbl.find h name in
          add rh name (c,Hashtbl.find hv v);
          (name,Some(c,Hashtbl.find hv v))
        end 
      ) disjunction
    )
  in
  let pkglist = 
    Cudf.fold_packages (fun acc pkg ->
      let hv = try Hashtbl.find h pkg.Cudf.package with Not_found -> assert false in
      let p = 
        { pkg with
          Cudf.version = (try Hashtbl.find hv pkg.Cudf.version with Not_found -> assert false);
          Cudf.depends = (try cnf_map h pkg.Cudf.depends with Not_found -> assert false);
          Cudf.conflicts = (try conj_map h pkg.Cudf.conflicts with Not_found -> assert false);
          Cudf.provides = (try conj_map h pkg.Cudf.provides with Not_found -> assert false)
        }
      in p::acc
    ) [] universe 
  in (rh,pkglist)
;;

let interesting_future_versions p sels real_versions =
  let evalsel v = function
      (`Eq,v') -> v=v'
    | (`Geq,v') -> v>=v'
    | (`Leq,v') -> v<=v'
    | (`Gt,v') -> v>v'
    | (`Lt,v') -> v<v'
    | (`Neq,v') -> v<>v'
  in
  let is_real = Hashtbl.mem p real_versions
  in
  let rawvl =
    if is_real
    then
      let pv = Hashtbl.find p real_versions in
	List.filter
	  (fun v -> (v > pv))
	  (ExtLib.List.unique (List.map snd sels))
    else ExtLib.List.unique (List.map snd sels)
  in
  let minv,maxv=
    List.fold_left
      (fun (mi,ma) v -> (min mi v,max ma v)) (List.hd rawvl,List.hd rawvl)
      (List.tl rawvl)
  in
  let h = Hashtbl.create 17
  and h' = Hashtbl.create 17 in
    begin
      if is_real then
	let pv = Hashtbl.find p real_versions
	in Hashtbl.add h (List.map (evalsel pv) sels) pv
    end;
    for offs = 0 to (maxv-minv+2) do
      let w = maxv+1-offs in
      let row = List.map (evalsel w) sels in
	if not (Hashtbl.mem h row) then 
          (Hashtbl.add h row w; Hashtbl.add h' w row);
    done;
    Hashtbl.fold (fun k v acc -> k::acc) h' []
;;


(****************************************************************************)

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
  and purged_universe = purge_universe complete_universe in
  let sync_table = synchronization_table purged_universe 
  and (sels,renumbered_packages) = renumber purged_universe 
  in
  let pl =
    let pinned_real_packages =
      List.map
	(fun p -> {p with
		     Cudf.provides = ("@source-"^(sourcename_of_package p),Some (`Eq, p.Cudf.version))::p.Cudf.provides;
		     Cudf.conflicts = ("@source-"^(sourcename_of_package p),Some (`Neq, p.Cudf.version))::p.Cudf.conflicts;
		  })
	renumbered_packages
    and real_versions = Hashtbl.create (List.length renumbered_packages)
    in
      List.iter
	(fun p -> Hashtbl.add real_versions p.Cudf.package p.Cudf.version)
	renumbered_packages;
      Hashtbl.fold
	(fun p sels acc ->
	   let sync = 
	     try Some (Hashtbl.find sync_table p.Cudf.package)
	     with Not_found -> None
	   in
	     (List.map
	       (fun v ->
		  {Cudf.package = p.Cudf.package;
		   Cudf.version = v;
		   Cudf.depends = [];
		   Cudf.provides = (match sync with Some s -> ["@source-"^s, Some (`Eq, v)] | None -> []);
		   Cudf.conflicts = (match sync with Some s -> ["@source-"^s, Some (`Neq, v)] | None -> []);
		   Cudf.installed = false;
		   Cudf.was_installed = false;
		   Cudf.keep = `Keep_none;
		   Cudf.pkg_extra = []
		  })
	       (interesting_future_versions p.Cudf.package sels real_versions))@acc
	)
	sels
	pinned_real_packages
  in
  let universe = Cudf.load_universe pl in 
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

