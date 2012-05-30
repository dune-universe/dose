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

(** Debian Specific Cudf conversion routines *)

open ExtLib
open Common
open Packages

include Util.Logging(struct let label = __FILE__ end) ;;
module SMap = Map.Make (String)

type tables = {
  virtual_table : unit Util.StringHashtbl.t;
  unit_table : unit Util.StringHashtbl.t ;
  versions_table : int Util.StringHashtbl.t;
  versioned_table : unit Util.StringHashtbl.t;
  reverse_table : (string SMap.t) ref Util.IntHashtbl.t
}

let create n = {
  virtual_table = Util.StringHashtbl.create (10 * n);
  unit_table = Util.StringHashtbl.create (2 * n);
  versions_table = Util.StringHashtbl.create (10 * n);
  versioned_table = Util.StringHashtbl.create (10 *n);
  reverse_table = Util.IntHashtbl.create (10 * n);
}

type lookup = {
  from_cudf : Cudf.package -> (string * string);
  to_cudf : (string * string) -> Cudf.package
}

type extramap = (string * (string * Cudf_types.typedecl1)) list

type options = {
  extras_opt : extramap ;
  native : string;
  host : string;
  build : string;
  foreign : string list ;
  ignore_essential : bool;
}

let default_options = {
  extras_opt = [] ;
  native = "";
  build = "";  (* the default architecture 'dpkg -print-architecture' *)
  host = "";   (* used to resolv cross dependencies *)
  foreign = []; (* list of foreign architectures *)
  ignore_essential = false
}

let add_name_arch a n = CudfAdd.encode (Printf.sprintf "%s:%s" n a)

let add_arch hostArch arch = function
  |n when String.ends_with n ":any" -> (CudfAdd.encode n)
  |n when arch = "all" -> add_name_arch n hostArch
  |n -> add_name_arch n arch

let add_arch_l hostArch arch l = 
  List.map (fun (n,c) -> (add_arch hostArch arch n,c)) l

let clear tables =
  Util.StringHashtbl.clear tables.virtual_table;
  Util.StringHashtbl.clear tables.unit_table;
  Util.StringHashtbl.clear tables.versions_table;
  Util.StringHashtbl.clear tables.versioned_table;
  Util.IntHashtbl.clear tables.reverse_table
;;

let add table k v =
  if not(Util.StringHashtbl.mem table k) then
    Util.StringHashtbl.add table k v

let add_v table k v =
  if not(Hashtbl.mem table k) then
    Hashtbl.add table k v

(* collect names of virtual packages *)
let init_virtual_table table pkg =
  List.iter (fun ((name,_),_) -> add table name ()) pkg.provides

(* collect names of real packages *)
let init_unit_table table pkg = 
  add table pkg.name ()

(* collect all versions mentioned of depends, pre_depends, conflict and breaks *)
let init_versioned_table table pkg =
  let conj_iter l = List.iter (fun ((name,_),_)-> add table name ()) l in
  let cnf_iter ll = List.iter conj_iter ll in
  conj_iter pkg.conflicts ;
  conj_iter pkg.breaks ;
  cnf_iter pkg.pre_depends ;
  cnf_iter pkg.depends
;;

(* collect all versions mentioned anywhere in the universe, including source
   fields *)
let init_versions_table table pkg =
  let conj_iter l =
    List.iter (fun ((name,_),sel) ->
      match CudfAdd.cudfop sel with
      |None -> ()
      |Some(_,version) -> add_v table (version,name) ()
    ) l
  in
  let cnf_iter ll = List.iter conj_iter ll in
  let add_source pv = function
    |(p,None) -> add_v table (pv,p) ()
    |(p,Some(v)) -> add_v table (v,p) ()
  in 
  add_v table (pkg.version,pkg.name) ();
  conj_iter pkg.breaks;
  conj_iter pkg.provides;
  conj_iter pkg.conflicts ;
  conj_iter pkg.replaces;
  cnf_iter pkg.depends;
  cnf_iter pkg.pre_depends;
  cnf_iter pkg.recommends;
  add_source pkg.version pkg.source
;;

let init_tables ?(options=default_options) ?(step=1) ?(versionlist=[]) pkglist =
  let n = List.length pkglist in
  let tables = create n in 
  let temp_versions_table = Hashtbl.create (10 * n) in
  let ivt = init_versions_table temp_versions_table in
  let ivrt = init_virtual_table tables.virtual_table in
  let ivdt = init_versioned_table tables.versioned_table in
  let iut = init_unit_table tables.unit_table in

  List.iter (fun v -> add_v temp_versions_table (v,"") ()) versionlist; 
  List.iter (fun pkg -> ivt pkg ; ivrt pkg ; ivdt pkg ; iut pkg) pkglist ;
  let l = Hashtbl.fold (fun v _ acc -> v::acc) temp_versions_table [] in
  let add_reverse i (n,v) =
     try 
       let m = Util.IntHashtbl.find tables.reverse_table i in 
       m := (SMap.add n v !m)
     with Not_found ->
       let m = SMap.add n v SMap.empty in
       Util.IntHashtbl.add tables.reverse_table i (ref m)
  in
  let sl = List.sort ~cmp:(fun x y -> Version.compare (fst x) (fst y)) l in
  let rec numbers (prec,i) = function
    |[] -> ()
    |(v,n)::t ->
      if Version.equal v prec then begin
        add tables.versions_table v i;
        add_reverse i (n,v);
        numbers (prec,i) t
      end else begin
        add tables.versions_table v (i+step);
        add_reverse (i+step) (n,v);
        numbers (v,(i+step)) t
      end
  in
  (* versions start from 1 *)
  numbers ("",1) sl;
  tables
;;

let get_cudf_version tables (package,version) =
  try Util.StringHashtbl.find tables.versions_table version
  with Not_found -> begin
    warning "Package (%s,%s) does not have an associated cudf version" package version;
    raise Not_found
  end

let get_real_version tables (name,cudfversion) =
  let package = 
    (* XXX this is a hack. I should record the name with the architecture *)
    let n = CudfAdd.decode name in
    try snd(ExtString.String.split n ":") 
    with Invalid_string _ -> n
  in
  try
    let m = !(Util.IntHashtbl.find tables.reverse_table cudfversion) in
    try SMap.find package m 
    with Not_found ->
      let known =
        String.concat "," (
          List.map (fun (n,v) ->
            Printf.sprintf "(%s,%s)" n v
          ) (SMap.bindings m)
        )
      in
      fatal "Unable to get real version for %s\n All Known versions for this package are %s" package known
  with Not_found ->
    fatal "Package (%s,%d) does not have an associated debian version" name cudfversion

let loadl tables l =
  List.flatten (
    List.map (fun ((name,_),sel) ->
      let encname = (* CudfAdd.encode *) name in
      match CudfAdd.cudfop sel with
      |None ->
          if (Util.StringHashtbl.mem tables.virtual_table name) &&
          (Util.StringHashtbl.mem tables.versioned_table name) then
            [(encname, None);("--virtual-"^encname, None)]
          else
            [(encname, None)]
      |Some(op,v) ->
          [(encname,Some(op,get_cudf_version tables (name,v)))]
    ) l
  )

(* we add a self conflict here, because in debian each package is in conflict
   with all other versions of the same package *)
let loadlc tables name l = (CudfAdd.encode name, None)::(loadl tables l)

let loadlp tables l =
  List.map (fun ((name,_),sel) ->
    let encname = (* CudfAdd.encode *) name in
    match CudfAdd.cudfop sel with
    |None  ->
        if (Util.StringHashtbl.mem tables.unit_table name) || 
        (Util.StringHashtbl.mem tables.versioned_table name)
        then ("--virtual-"^encname,None)
        else (encname, None)
    |Some(`Eq,v) ->
        if (Util.StringHashtbl.mem tables.unit_table name) || 
        (Util.StringHashtbl.mem tables.versioned_table name)
        then ("--virtual-"^encname,Some(`Eq,get_cudf_version tables (name,v)))
        else (encname,Some(`Eq,get_cudf_version tables (name,v)))
    |_ -> fatal "This should never happen : a provide can be either = or unversioned"
  ) l

let loadll tables ll = List.map (loadl tables) ll

(* ========================================= *)

let preamble = 
  (* number is a mandatory property -- no default *)
  let l = [
    ("replaces",(`Vpkglist (Some [])));
    ("recommends",(`Vpkgformula (Some [])));
    ("number",(`String None));
    ("architecture",(`String None));
    ("priority",(`String (Some "")));
    ("source",(`String (Some ""))) ;
    ("sourcenumber",(`String (Some "")));
    ("sourceversion",(`Int (Some 1))) ;
    ("essential",(`Bool (Some false))) ;
    ("buildessential",(`Bool (Some false))) ;
    ]
  in
  CudfAdd.add_properties Cudf.default_preamble l

let add_extra_default extras tables pkg =
  let number = ("number",`String pkg.version) in
  let architecture = ("architecture",`String pkg.architecture) in
  let priority = ("priority",`String pkg.priority) in
  let essential = ("essential", `Bool pkg.essential) in
  let build_essential = ("buildessential", `Bool pkg.build_essential) in
  let (source,sourcenumber,sourceversion) =
    let (n,v) =
      match pkg.source with
      |("",_) -> (pkg.name,pkg.version)
      |(n,None) -> (n,pkg.version)
      |(n,Some v) -> (n,v)
    in
    let cv = get_cudf_version tables ("",v) in
    ("source",`String n), ("sourcenumber", `String v), ("sourceversion", `Int cv)
  in
  let l =
    List.filter_map (fun (debprop, (cudfprop,v)) ->
      try 
        let s = Packages.assoc (String.lowercase debprop) pkg.extras in
        let typ = Cudf_types.type_of_typedecl v in
        Some (cudfprop, Cudf_types_pp.parse_value typ s)
      with Not_found -> None
    ) extras
  in
  let recommends = ("recommends", `Vpkgformula (loadll tables pkg.recommends)) in
  let replaces = ("replaces", `Vpkglist (loadl tables pkg.replaces)) in
  List.filter_map (function
    |(_,`Vpkglist []) -> None
    |(_,`Vpkgformula []) -> None
    |(_,`String "") -> None
    |e -> Some e
  )
  [priority; architecture; number;
  source; sourcenumber; sourceversion; 
  recommends; replaces;
  essential;build_essential]@ l
;;

let add_essential = function
  |false -> `Keep_none
  |true -> `Keep_package

let add_inst inst pkg =
  if inst then true 
  else
    try
      match String.nsplit (Packages.assoc "status" pkg.extras) " " with
      |[_;_;"installed"] -> true
      | _ -> false
    with Not_found -> false

let add_extra extras tables pkg =
  add_extra_default extras tables pkg

let tocudf tables ?(options=default_options) ?(inst=false) pkg =
  if options.native <> "" then begin
    let _name = add_arch options.native pkg.architecture pkg.name in
    let version = get_cudf_version tables (pkg.name,pkg.version)  in
    let _provides = 
      let l = 
        match pkg.multiarch with
        |`None -> [(CudfAdd.encode pkg.name,None)]
        |`Foreign -> List.map (fun arch -> (add_arch options.native arch pkg.name,Some(`Eq,version))) options.foreign
        |`Allowed -> [(CudfAdd.encode pkg.name,None) ; (CudfAdd.encode (pkg.name^":any"),None)]
        |`Same -> []
      in
      l@(add_arch_l options.native pkg.architecture (loadlp tables pkg.provides))
    in
    let _conflicts = 
      (* self conflict / multi-arch conflict *)
      let sc = (add_arch options.native pkg.architecture pkg.name,None) in
      let mac = (CudfAdd.encode pkg.name,None) in
      let l = pkg.breaks @ pkg.conflicts in
      match pkg.multiarch with
      |(`None|`Foreign|`Allowed) -> 
          sc::mac::(add_arch_l options.native pkg.architecture (loadl tables l))
      |`Same -> sc::(add_arch_l options.native pkg.architecture (loadl tables l))
    in
    let _depends = 
      List.map (add_arch_l options.native pkg.architecture) 
      (loadll tables (pkg.pre_depends @ pkg.depends))
    in
    { Cudf.default_package with
      Cudf.package = _name ;
      Cudf.version = get_cudf_version tables (pkg.name,pkg.version) ;
      Cudf.keep = if options.ignore_essential then `Keep_none else add_essential pkg.essential;
      Cudf.depends = _depends;
      Cudf.conflicts = _conflicts ;
      Cudf.provides = _provides ;
      Cudf.installed = add_inst inst pkg;
      Cudf.pkg_extra = add_extra options.extras_opt tables pkg ;
    }
  end else
    { Cudf.default_package with
      Cudf.package = CudfAdd.encode pkg.name ;
      Cudf.version = get_cudf_version tables (pkg.name,pkg.version) ;
      Cudf.keep = if options.ignore_essential then `Keep_none else add_essential pkg.essential;
      Cudf.depends = loadll tables (pkg.pre_depends @ pkg.depends);
      Cudf.conflicts = loadlc tables pkg.name (pkg.breaks @ pkg.conflicts) ;
      Cudf.provides = loadlp tables pkg.provides ;
      Cudf.installed = add_inst inst pkg;
      Cudf.pkg_extra = add_extra options.extras_opt tables pkg ;
    }

let lltocudf = loadll
let ltocudf = loadl

let load_list ?options l =
  let timer = Util.Timer.create "Debian.Debcudf.load_list" in
  Util.Timer.start timer;
  let tables = init_tables ?options l in
  let pkglist = List.map (tocudf tables ?options) l in
  clear tables;
  Util.Timer.stop timer pkglist

let load_universe ?options l =
  let timer = Util.Timer.create "Debian.Debcudf.load_universe" in
  let pkglist = load_list ?options l in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
