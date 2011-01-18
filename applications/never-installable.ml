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
(* Parts of this code are canibalized from strongpred.ml.                   *)
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

let chop_binnmu s =
  (* chops a possible bin-NMU suffix from a debian version string *)
  try
    Str.string_before s
      (Str.search_backward (Str.regexp "\\+b[0-9]+$") s ((String.length s)-1))
  with
      Not_found -> s
;;

let chop_epoch s =
  (* chops a possible epoch from a debian version string *)
  if Str.string_match (Str.regexp "[0-9]+:") s 0
  then Str.string_after s (Str.match_end ())
  else s
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
  sourcename_of_package package,
  chop_epoch (chop_binnmu (debversion_of_package package))
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

(* returns two hash tables :                                                *)
(* - mapping each cluster s to the list of pairs (p,c) such that            *)
(*   there exists a constraint c on package name p belonging to cluster s   *)
(* - mapping each new package name p to the list of constraints such that   *)
(*   there exists a constraint c on package p.                              *)
let build_constraint_tables package_list cluster_table=
  let ctable = Hashtbl.create ((List.length package_list) / 2)
  and ntable = Hashtbl.create ((List.length package_list) / 100)
  and add table name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
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
       (fun (p,c) ->
	 try add ctable (Hashtbl.find cluster_table p) (p,c)
	 with Not_found ->  add ntable p c)
    )
    package_list;
  (ctable,ntable)
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
  let from_cudf p = from_cudf (p.Cudf.package,p.Cudf.version)
  and (purged_package_list, cudf_version_table) =
    purge_universe complete_universe
  in
  let size_of_cluster,cluster_of_package = 
    compute_clusters purged_package_list 
  and pp pkg =
    let (p,v) = from_cudf pkg in 
    let l = 
      ExtLib.List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["source";"sourceversion"]
    in (p,v,l)
  in
(* 
  let pl =
    let real_versions = Hashtbl.create (List.length renumbered_packages)
    in
      List.iter
	(fun p -> Hashtbl.add real_versions p.Cudf.package p.Cudf.version)
	renumbered_packages;
      Hashtbl.fold
	(fun p sels acc ->
	   let sync = 
	     try Some (Hashtbl.find sync_table p)
	     with Not_found -> None
	   in
	     (List.map
	       (fun v ->
		  {Cudf.package = p;
		   Cudf.version = v;
		   Cudf.depends = [];
		   Cudf.provides = (match sync with
		       Some (s,_sv) -> ["src:"^s, Some (`Eq, v)]
		     | None -> []);
		   Cudf.conflicts = (match sync with
		       Some (s,_sv) -> ["src:s"^s, Some (`Neq, v);p,None]
		     | None -> [p,None]);
		   Cudf.installed = false;
		   Cudf.was_installed = false;
		   Cudf.keep = `Keep_none;
		   Cudf.pkg_extra = []
		  })
	       (interesting_future_versions p !sels real_versions))@acc
	)
	constraint_table
	(List.map (pin_real sync_table) renumbered_packages)
  in
  info "Number of packages after renumbering: %i" (List.length pl);

*)

  let pl= List.map
    (synchronise_package size_of_cluster)
    purged_package_list
  in

  let constraints_on_clusters,constraints_on_missing_packages =
    build_constraint_tables pl cluster_of_package
  in
  let translation_table =
    ExtLib.Hashtbl.map 
      (fun constraints ->
	mapi
	  (fun i x -> x,(2*i)+2)
	  (ExtLib.List.unique
	     (ExtLib.List.sort
		(List.fold_left 
		   (fun acc cons -> match cons with
		     | None -> acc
		     | Some(rel,version) -> version::acc)
		   []
		   !constraints))))
      constraints_on_missing_packages
  in

  print_string "Clusters:\n";
  Hashtbl.iter
    (fun k v -> begin print_string (fst k); print_newline () end)
    constraints_on_clusters;
  print_string "Missing:\n";
  Hashtbl.iter
    (fun k v -> begin print_string k; print_newline () end)
    constraints_on_missing_packages;

  let future_missing_packages =
    let trivial_package = 
      {Cudf.package = "package name not instantiated";
       Cudf.version = 0;
       Cudf.depends = [];
       Cudf.conflicts = [];
       Cudf.provides = [];
       Cudf.installed = false;
       Cudf.was_installed = false;
       Cudf.keep = `Keep_none;
       Cudf.pkg_extra = []
      }
    in
    Hashtbl.fold
      (fun package_name translations accu ->
	List.fold_left
	  (fun accu (old_cudf_version,new_cudf_version) ->
	    {trivial_package with 
	      Cudf.package = package_name;
	      Cudf.version = new_cudf_version}
	    ::{trivial_package with
	      Cudf.package = package_name;
	      Cudf.version = new_cudf_version+1}
	    ::accu)
	  ({trivial_package with
	    Cudf.package = package_name;
	    Cudf.version = 1}
	   ::accu)
	  translations
      )
      translation_table
      []
  in
  
  let universe = Cudf.load_universe
    (future_missing_packages@(renumber_packages pl translation_table))
  in
  
  info "Solving..." ;
  let timer = Util.Timer.create "Solver" in
  Util.Timer.start timer;
  let explain = OptParse.Opt.get Options.explain in
  let fmt = Format.std_formatter in
  Format.fprintf fmt "@[<v 1>report:@,";
  let callback =
    Diagnostic.fprintf ~pp ~failure:true ~success:false ~explain fmt in
  let i = Depsolver.univcheck ~callback universe 
  in
    ignore(Util.Timer.stop timer ());

    Format.fprintf fmt "@]@.";
    Format.fprintf fmt "total-packages: %d\n" (Cudf.universe_size universe);
    Format.fprintf fmt "broken-packages: %d\n" i;
    if OptParse.Opt.is_set Options.architecture then
      Format.fprintf fmt "architecture: %s\n"
	(OptParse.Opt.get Options.architecture);
    
  if OptParse.Opt.is_set Options.cudf_output then
    let ch=open_out (OptParse.Opt.get Options.cudf_output)
    in begin
      Cudf_printer.pp_universe (Format.formatter_of_out_channel ch) universe;
      close_out ch
    end
;;

main () ;;

(*


  
(* This section is largely copied from strongpred.ml !! *)

(* map the package list to a pair of table {package_names->l}, where l is  *)
(* is the list of all constraints in which the package_name is mentionend, *)
(* and package list, such that all package numbers are spaced out.         *)
let renumber packages = 
  in let build_table packages =
      (* build a table that maps each package name to a map *)
      (* old version -> new version                         *)
      let version_table = Hashtbl.create (List.length packages) in
	List.iter (add_to_versions_table version_table) packages;
	let translation_table = Hashtbl.create (List.length packages) in
	  Hashtbl.iter
	    (fun p_name {contents=l} ->
	       let c = ref 1 in
	       let translation = Hashtbl.create (List.length l) in
		 List.iter
		   (fun p_version ->
		      Hashtbl.add translation p_version (2 * !c);
		      incr c 
		   )
		   (ExtLib.List.sort (ExtLib.List.unique l));
		 Hashtbl.add translation_table p_name translation
	    )
	    version_table;
	  translation_table
  in let translation_table = build_table packages
  and constraint_table = Hashtbl.create (List.length packages) in
  let clause_map translation_table =
    List.map (fun (name,sel) ->
      match sel with
      |None -> (name,sel)
      |Some(c,v) -> begin
        let translation = Hashtbl.find translation_table name in
	let new_v = Hashtbl.find translation v in
          add constraint_table name ((c :> Cudf_types.relop),new_v);
          (name,Some(c,new_v))
      end
    )
  in
  let pkglist = 
    List.map
      (fun pkg ->
	 let translation =
	   try Hashtbl.find translation_table pkg.Cudf.package
	   with Not_found -> assert false in
           { pkg with
               Cudf.version = (
		 try Hashtbl.find translation pkg.Cudf.version
		 with Not_found -> assert false);
               Cudf.depends = (
		 try List.map (clause_map translation_table) pkg.Cudf.depends
		 with Not_found -> assert false);
               Cudf.conflicts = (
		 try clause_map translation_table pkg.Cudf.conflicts
		 with Not_found -> assert false);
               Cudf.provides = (
		 try clause_map translation_table pkg.Cudf.provides
		 with Not_found -> assert false)
           }
      ) 
      packages
  in (constraint_table,pkglist)
;;

(* This is also copied from Roberto. *)

let interesting_future_versions p sels real_versions =
  let evalsel v = function
  (`Eq,v') -> v=v'
    | (`Geq,v') -> v>=v'
    | (`Leq,v') -> v<=v'
    | (`Gt,v') -> v>v'
    | (`Lt,v') -> v<v'
    | (`Neq,v') -> v<>v'
  in
  let is_real = Hashtbl.mem real_versions p
  in
  let rawvl =
    if is_real
    then begin
      let pv = Hashtbl.find real_versions p in
      List.filter
	(fun v -> (v > pv))
	(ExtLib.List.unique (List.map snd sels))
    end
    else begin
      ExtLib.List.unique (List.map snd sels)
    end
  in
  if List.length rawvl = 0
  then []
  else
    let minv,maxv=
      List.fold_left
	(fun (mi,ma) v -> (min mi v,max ma v)) (List.hd rawvl,List.hd rawvl)
	(List.tl rawvl)
    in
    let h = Hashtbl.create 17
    and h' = Hashtbl.create 17 in
    begin
      if is_real then
	let pv = Hashtbl.find real_versions p
	in Hashtbl.add h (List.map (evalsel pv) sels) pv
    end;
    for offs = 0 to (maxv-minv+2) do
      let w = maxv+1-offs in
      let row = List.map (evalsel w) sels in
      if not (Hashtbl.mem h row) then 
        (Hashtbl.add h row w; Hashtbl.add h' w row);
    done;
    Hashtbl.fold (fun k v acc -> k::acc) h' []

*)
