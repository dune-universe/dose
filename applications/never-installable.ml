(****************************************************************************)
(*  Copyright (C) 2010, 2011 Ralf Treinen <treinen@pps.jussieu.fr>          *)
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
  let cudf_output = StdOpt.str_option ()

  let description =
    "Report packages that aren't installable in any futures of a repository"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~long_name:"verbose"
    ~help:"Print additional information" verbose;
  add options ~short_name:'e' ~long_name:"explain"
    ~help:"Explain the results" explain;
  add options ~short_name:'a' ~long_name:"architecture"
    ~help:"Set the default architecture" architecture;
  add options ~long_name:"cudf-to"
    ~help:"Dump CUDF to file" cudf_output;
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


(* normalize a debian version string by removing epoch and bin-NMU *)
let normalize s =
  let chop_binnmu s =
  (* chops a possible bin-NMU suffix from a debian version string *)
    try
      Str.string_before s
	(Str.search_backward (Str.regexp "\\+b[0-9]+$") s ((String.length s)-1))
    with
	Not_found -> s
  and chop_epoch s =
  (* chops a possible epoch from a debian version string *)
    if Str.string_match (Str.regexp "[0-9]+:") s 0
    then Str.string_after s (Str.match_end ())
    else s
  in
  chop_epoch (chop_binnmu s)
;;

let rec pos_in_list x = function
  | h::r -> if h=x then 0 else 1+(pos_in_list x r)
  | [] -> raise Not_found
;;
(****************************************************************************)

(* (purge_universe universe) returns the list of packages in universe that  *)
(* is obtained by retaing only the highest version of each binary package,  *)
(* and a hashtable that maps each package name to the latest version.       *)
(* Prints a warning for each surpressed package.                            *)
let purge_universe universe =

  let cudf_versions = Hashtbl.create (Cudf.universe_size universe)
  (* maps each binary package name to the latest cudf version *)
  and cruft = Hashtbl.create ((Cudf.universe_size universe)/100)
  (* maps each obsolete (package name, package cudf version) to the *)
  (* newer debian version *)
  in
  
  (* identify cruft *)
  Cudf.iter_packages
    (fun p ->
      let name = p.Cudf.package
      and cudf_version = p.Cudf.version
      and deb_version = debversion_of_package p
      in begin
	try
	  let old_cudf_version = Hashtbl.find cudf_versions name
	  in 
	  if old_cudf_version < cudf_version 
	  then begin
	    Hashtbl.add cruft (name,old_cudf_version) deb_version;
	    Hashtbl.replace cudf_versions name cudf_version;
	  end
	  else if cudf_version < old_cudf_version
	  then
	    let old_deb_version = debversion_of_package
	      (Cudf.lookup_package universe (name,old_cudf_version))
	    in Hashtbl.add cruft (name,cudf_version) old_deb_version
	  else failwith "two packages with same (name,version)"
	with Not_found -> Hashtbl.add cudf_versions name cudf_version;
      end)
    universe;
  
  (* filter out cruft *)
  let package_list =
    filter_packages
      (fun p ->
	let name = p.Cudf.package
	and version = p.Cudf.version
	and deb_version = debversion_of_package p
	in
	try
	  warning
	    "%s(%s) dropped: %s is newer."
	    name deb_version (Hashtbl.find cruft (name,version));
	  false
	with Not_found -> true
      )
      universe
  in (package_list,cudf_versions)
;;

(**************************************************************************)

(* The cluster of a binary package p is the pair (s,w) where              *)
(* -s is the source package of p;                                         *)
(* -w is the debian version of p without epoch and bin-nmu.               *)

let cluster_of package = (
  sourcename_of_package package, normalize (debversion_of_package package)
);;

(* returns to hash tables:                                               *)
(* - mapping each cluster to its size                                    *)
(* - mapping each binary package name to its cluster                     *)
let compute_clusters package_list =
  let number_of_packages = List.length package_list
  in
  let size_table = Hashtbl.create (number_of_packages/2)
  and cluster_table = Hashtbl.create number_of_packages
  in
  List.iter
    (fun package ->
      let cluster = cluster_of package
      in
      Hashtbl.add cluster_table package.Cudf.package cluster;
      try let oldcount = Hashtbl.find size_table cluster
	  in Hashtbl.replace size_table cluster (oldcount+1)
      with Not_found -> Hashtbl.add size_table cluster 1
    )
    package_list;
  size_table,cluster_table
;;

(* add to a package the constraints that synchronise it with its cluster *)
let synchronise_package cluster_size_table package  =
  let (s,w) as cluster = cluster_of package
  in
  if Hashtbl.find cluster_size_table cluster > 1
  then
    let clustername = "src:"^s^":"^w
    and clusterversion = 1
    in
    {package with
      Cudf.provides = 
	(clustername, Some (`Eq, clusterversion))::package.Cudf.provides;
      Cudf.conflicts =
	(clustername, Some (`Neq, clusterversion))::package.Cudf.conflicts; 
       }
  else package
;;

(****************************************************************************)

(* renumber all packages mentioned in the package list in any of the fields *)
(* version, conflicts, depends, provides.                                   *)
(* translation is a hash table mapping a package name to an association     *)
(* list that maps old version numbers to new version numbers.               *)
let renumber_packages package_list translation =
  let translate_version package version =
    try
      List.assoc version (Hashtbl.find translation package)
    with
	Not_found -> version
  in
  let translate_constrained = function
    | (name,None) as c -> c
    | (name,Some(relation,version)) ->
      (name,Some(relation,translate_version name version))
  in
  List.map
    (fun p -> { p with 
      Cudf.version = translate_version p.Cudf.package p.Cudf.version;
      Cudf.conflicts = List.map translate_constrained p.Cudf.conflicts;
      Cudf.depends = List.map (List.map translate_constrained) p.Cudf.depends;
     }
    )
    package_list
;;

(****************************************************************************)

let mapi f = 
  let rec mapi_aux i = function
    | [] -> []
    | h::r -> (f i h)::(mapi_aux (i+1) r)
  in mapi_aux 0
;;

let main () =
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
  let (purged_package_list, cudf_version_table) =
    purge_universe complete_universe
  in
  let size_of_cluster,cluster_of_package = 
    compute_clusters purged_package_list 
  and pp pkg =
    let (p,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in 
    let l = 
      ExtLib.List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["source";"sourceversion"]
    in (p,v,l)
  in
  
  let referred_versions_of_package = 
    Hashtbl.create
      (let n = List.length purged_package_list in n + n/10)
  and normalized_debian_versions_of_cluster =
    Hashtbl.create (Hashtbl.length size_of_cluster)
  in
  let add table name version =
    try let l = Hashtbl.find table name	in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  and add_without_version table name =
    if not (Hashtbl.mem table name)
    then Hashtbl.add table name (ref [])
  and iter_constraints f package =
    begin
      List.iter 
	(function clause -> List.iter f clause)
	package.Cudf.depends;
      List.iter f package.Cudf.conflicts
    end
  in
  List.iter
    (iter_constraints
       (fun (package,constr) ->
	 match constr with
	   | None -> add_without_version referred_versions_of_package package
	   | Some(_rel,version) ->
	     try
	       let existing_version =
		 Hashtbl.find cudf_version_table package
	       in
	       if version > existing_version then begin
		 add referred_versions_of_package package version;
		 add normalized_debian_versions_of_cluster
		   (Hashtbl.find cluster_of_package package)
		   (normalize (snd (from_cudf(package,version))))
	       end
	     with Not_found -> add referred_versions_of_package package version
       ))
    purged_package_list;
  
  Hashtbl.iter
    (fun _package versions ->
      let l = ExtLib.List.unique 
	(ExtLib.List.sort ~cmp:Debian.Version.compare (!versions ))
      in versions := l)
    normalized_debian_versions_of_cluster;

  Hashtbl.iter
    (fun (s,v) versions ->
      print_string s;
      print_char ':';
      print_string v;
      print_string ": ";
      List.iter
	(fun v -> print_string v; print_string ", ")
	(!versions);
      print_newline ()
    )
    normalized_debian_versions_of_cluster;
  
  let translation_table = Hashtbl.create
    (Hashtbl.length referred_versions_of_package)
  in
  Hashtbl.iter 
    (fun package cudf_versions ->
      if Hashtbl.mem cudf_version_table package
      then 
	Hashtbl.add translation_table package
	  (((Hashtbl.find cudf_version_table package),1)::
	      (List.map
		 (fun cudf_version ->
		   let n =
		     pos_in_list 
		       (normalize (snd (from_cudf (package,cudf_version))))
		       (!(Hashtbl.find
			    normalized_debian_versions_of_cluster
			    (Hashtbl.find cluster_of_package package)))
		   in (cudf_version,2*n+3)
		 )
		 !cudf_versions))
      else
	Hashtbl.add translation_table package
	  (mapi
	     (fun i x -> x,(2*i)+2)
	     (ExtLib.List.unique (ExtLib.List.sort !cudf_versions)))
    )
    referred_versions_of_package;

  
  let new_cudf_to_debian = Hashtbl.create (Hashtbl.length translation_table)
  in
  Hashtbl.iter
    (fun package_name translations ->
      Hashtbl.add new_cudf_to_debian package_name
	(if Hashtbl.mem cudf_version_table package_name
	 then (* we have a package with that name in the universe *)
	    let current_debian_version =
	      snd(from_cudf(package_name,
			    Hashtbl.find cudf_version_table package_name))
	    and deb_versions =
	      try !(Hashtbl.find
		      normalized_debian_versions_of_cluster
		      (Hashtbl.find cluster_of_package package_name))
	      with Not_found -> []
	    in let rec f current_cudf previous_debian_version = function
	      | h::r -> 
		(current_cudf,"("^previous_debian_version^".."^h^")")
		::(current_cudf+1,h)
		::(f (current_cudf+2) h r)
	      | [] ->
		[(2*List.length deb_versions+2,
		  "("^previous_debian_version^"..)")
		]
	       in
	       (1,current_debian_version)
	       ::(f 2 current_debian_version deb_versions)
	 else (* there is no package with that name in the universe *)
	    if translations=[]
	    then [(1,"(..)")]
	    else
	      let (highest_debian_version, accu) =
		List.fold_left
		  (fun
		    (previous_debian_version, accu)
		    (old_cudf_version, new_cudf_version) ->
		      let debian_version =
			snd(from_cudf(package_name,old_cudf_version))
		      in
		      (debian_version,
		       ((new_cudf_version-1),
			("("^previous_debian_version^".."^debian_version^")"))
		       ::(new_cudf_version,debian_version)
		       ::accu))
		  ("",[])
		  translations
	      in
	      (2*(List.length translations)+1,"("^highest_debian_version^"..)")
	      ::accu
	))
    translation_table;
  
  Hashtbl.iter
    (fun package translations ->
      print_string package;
      print_char ' ';
      (List.iter
	 (fun (cudf,deb) ->
	   print_int cudf; print_char '=';print_string deb;print_char ' ')
	 translations);
      print_newline ())
    new_cudf_to_debian;

  let future_packages =
    let make_package package_name cudf_version debian_version =
      {Cudf.package = package_name;
       Cudf.version = cudf_version;
       Cudf.depends = [];
       Cudf.conflicts = [];
       Cudf.provides = [];
       Cudf.installed = false;
       Cudf.was_installed = false;
       Cudf.keep = `Keep_none;
       Cudf.pkg_extra = [("number",`String debian_version)] }
    in
    Hashtbl.fold
      (fun package_name translations accu ->
	(List.map
	   (fun (cudf_version, debian_version) ->
	     (make_package package_name cudf_version debian_version))
	   translations)
	@ accu)
      new_cudf_to_debian
      []
  in

  let pl= List.map
    (synchronise_package size_of_cluster)
    purged_package_list
  in

  (*
  let universe = Cudf.load_universe
    (future_packages@(renumber_packages pl translation_table))
  in

  begin
    if OptParse.Opt.is_set Options.cudf_output then
      let ch=open_out (OptParse.Opt.get Options.cudf_output)
      in begin
	Cudf_printer.pp_universe (Format.formatter_of_out_channel ch) universe;
	close_out ch
      end
  end;

  info "Solving..." ;
  let timer = Util.Timer.create "Solver" in
  Util.Timer.start timer;
  let explain = OptParse.Opt.get Options.explain in
  let fmt = Format.std_formatter in
  Format.fprintf fmt "@[<v 1>report:@,";
  let callback =
    Diagnostic.fprintf ~pp ~failure:false ~success:false ~explain fmt in
  let i = Depsolver.univcheck ~callback universe 
  in
    ignore(Util.Timer.stop timer ());

    Format.fprintf fmt "@]@.";
    Format.fprintf fmt "total-packages: %d\n" (Cudf.universe_size universe);
    Format.fprintf fmt "broken-packages: %d\n" i;
    if OptParse.Opt.is_set Options.architecture then
      Format.fprintf fmt "architecture: %s\n"
	(OptParse.Opt.get Options.architecture)
  *) ()
;;
    

main () ;;

