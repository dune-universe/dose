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

module SSet = Set.Make(String)

open ExtLib
open Common
open Packages

include Util.Logging(struct let label = __FILE__ end) ;;
module SMap = Map.Make (String)

type tables = {
  virtual_table : SSet.t ref Util.StringHashtbl.t; (** all names of virtual packages *)
  unit_table : unit Util.StringHashtbl.t ;         (** all names of real packages    *)
  versions_table : int Util.StringHashtbl.t;       (** version -> int table          *)
  reverse_table : (string SMap.t) ref Util.IntHashtbl.t;
  versioned_table : unit Util.StringHashtbl.t;     (** all (versions,name) tuples. 
                                                       all versions mentioned of depends, 
                                                       pre_depends, conflict and breaks    *)
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
  native : string option; (* the native architecture *)
  foreign : string list ; (* list of foreign architectures *)
  host : string option;   (* the host architecture - cross compile *)
  ignore_essential : bool;
}

let default_options = {
  extras_opt = [] ;
  native = None;
  foreign = [];
  host = None;
  ignore_essential = false
}

let add_name_arch n a = CudfAdd.encode (Printf.sprintf "%s:%s" n a)
let add_arch native_arch name = function
  |"all" -> add_name_arch name native_arch
  |package_arch -> add_name_arch name package_arch
;;

(** add arch info to a vpkg 
 - if it's a :any dependency then just encode the name without arch information :
   means that this dependency/conflict can be satified by any packages
 - if it is a :native dependency then add the native architecture
 - if it is a package:arch dependency, then encode it as such
 - if the package dependency does not have any annotations :
   + if the package is architecture all, then all dependencies are interpreted as
     dependencies on native architecture packages.
   + otherwise all dependencies are satisfied by packages of the same architecture
     of the package we are considering 
Pcre.regexp "^(.* )(:(any|native|.* )){1}$" ;;
*)
let add_arch_deps native_arch package_arch name = 
  try 
    match String.split name ":" with
    |n,"any" -> CudfAdd.encode n
    |n,"native" -> add_name_arch n native_arch
    |n,a -> add_name_arch n a
  with ExtString.Invalid_string ->
    add_arch native_arch name package_arch
;;

let add_arch_l ?(deps=false) native_arch package_arch l = 
  if deps then
    List.map (fun (n,c) -> ((add_arch_deps native_arch package_arch n),c)) l
  else
    List.map (fun (n,c) -> ((add_arch native_arch n package_arch),c)) l
;;

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
  if not(Util.StringPairHashtbl.mem table k) then
    Util.StringPairHashtbl.add table k v

let add_s h k v =
  try let s = Util.StringHashtbl.find h k in s := SSet.add v !s
  with Not_found -> Util.StringHashtbl.add h k (ref (SSet.singleton v))
;;

(* collect names of virtual packages 
 * associates to each virtual package a list of real packages *)
let init_virtual_table table pkg =
  List.iter (fun ((name,_),_) -> add_s table name pkg.name) pkg.provides

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

(* collect all versions mentioned anywhere in the universe, including source fields *)
let init_versions_table table pkg =
  let conj_iter l =
    List.iter (fun ((name,_),constr) ->
      match CudfAdd.cudfop constr with
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

let init_tables ?(step=1) ?(versionlist=[]) pkglist =
  let n = List.length pkglist in
  let tables = create n in 
  let temp_versions_table = Util.StringPairHashtbl.create (10 * n) in
  let ivrt = init_virtual_table tables.virtual_table in
  (* XXX init_versions_table and init_versioned_table can be done at the same time !!! *)
  let ivt = init_versions_table temp_versions_table in
  let ivdt = init_versioned_table tables.versioned_table in
  let iut = init_unit_table tables.unit_table in

  List.iter (fun v -> add_v temp_versions_table (v,"") ()) versionlist; 
  List.iter (fun pkg -> ivt pkg ; ivrt pkg ; ivdt pkg ; iut pkg) pkglist ;
  let l = Util.StringPairHashtbl.fold (fun v _ acc -> v::acc) temp_versions_table [] in
  let add_reverse i (n,v) =
    debug "Add Reverse (%s,%s) %i" n v i;
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
      debug "Numbers %s %s" n v;
      if Version.equal v prec then begin
        add tables.versions_table v i;
        if n <> "" then add_reverse i (n,v);
        numbers (prec,i) t
      end else begin
        add tables.versions_table v (i+step);
        if n <> "" then add_reverse (i+step) (n,v);
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
    try
      let (n,a) = ExtString.String.split n ":" in
      if n = "src" then a else n
    with Invalid_string -> n
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
      fatal "Unable to get real version for %s (%i)\n All Known versions for this package are %s" package cudfversion known 
  with Not_found ->
    fatal "Package (%s,%d) does not have an associated debian version" name cudfversion

let loadl ?(enc=false) tables l =
  List.flatten (
    List.map (fun ((name,aop),constr) ->
      let encname = 
        let n = match aop with Some a -> name^":"^a | None -> name in
        if enc then CudfAdd.encode n else n
      in
      match CudfAdd.cudfop constr with
      |None ->
          if (Util.StringHashtbl.mem tables.virtual_table name) &&
          (Util.StringHashtbl.mem tables.versioned_table name) then
            [(encname, None);("--virtual-"^encname, None)]
          else
            [(encname, None)]
      |Some(op,v) ->
          (* debian policy 7.5 . If a relationship field has 
           * a version number attached, only real packages will 
           * be considered to see whether the relationship is satisfied *)
          if (Util.StringHashtbl.mem tables.virtual_table name) && 
             not (Util.StringHashtbl.mem tables.versioned_table name) then []
          else begin
            debug "Conflict %s < %s %s" encname name v;
            [(encname,Some(op,get_cudf_version tables (name,v)))]
          end
    ) l
  )

let loadll ?(enc=false) tables ll =
  List.filter_map (fun l -> match loadl ~enc tables l with [] -> None |l -> Some l) ll

(* we add a self conflict here, because in debian each package is in conflict
   with all other versions of the same package *)
let loadlc ?(enc=false) tables name l =
  let sc = if enc then (CudfAdd.encode name, None) else (name, None) in
  sc::(loadl ~enc tables l)

let loadlp ?(enc=false) tables l =
  List.map (fun ((name,_),constr) ->
    let encname = if enc then CudfAdd.encode name else name in
    match CudfAdd.cudfop constr with
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


(* ========================================= *)

let preamble = 
  let l = [
    (* name,number,type,architecture are mandatory properties -- no default *)
    ("name", (`String None));
    ("number",(`String None));
    ("type",(`String None));
    ("architecture",(`String None));

    ("replaces",(`Vpkglist (Some [])));
    ("recommends",(`Vpkgformula (Some [])));
    ("priority",(`String (Some "")));
    ("source",(`String (Some ""))) ;
    ("sourcenumber",(`String (Some "")));
    ("sourceversion",(`Int (Some 1))) ;
    ("essential",(`Bool (Some false))) ;
    ("buildessential",(`Bool (Some false))) ;
    ("filename",(`String (Some "")));
    ("installedsize",(`Int (Some 0)));
    ("multiarch",(`String (Some "")));
    ]
  in
  CudfAdd.add_properties Cudf.default_preamble l

let add_extra_default extras tables pkg =
  let check = function [] :: _ -> failwith (Printf.sprintf "Malformed dep (%s %s)" pkg.name pkg.version) |l -> l in
  let name = if String.starts_with pkg.name "src:" then
      String.sub pkg.name 4 ((String.length pkg.name) - 4)
    else pkg.name
  in
  let name = ("name",`String name) in
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
  let recommends = ("recommends", `Vpkgformula (check (loadll ~enc:true tables pkg.recommends))) in
  let replaces = ("replaces", `Vpkglist (loadl tables ~enc:true pkg.replaces)) in
  let extras = 
    ("Type",("type",`String None))::
    ("Filename",("filename",`String None))::
    ("Multi-Arch",("multiarch",`String None))::
    ("Installed-Size",("installedsize",`Int (Some 0)))::
    extras 
  in
  let l =
    List.filter_map (fun (debprop, (cudfprop,v)) ->
      try 
        let s = Packages.assoc debprop pkg.extras in
        let typ = Cudf_types.type_of_typedecl v in
        Some (cudfprop, Cudf_types_pp.parse_value typ s)
      with Not_found -> None
    ) extras
  in
  List.filter_map (function
    |(_,`Vpkglist []) -> None
    |(_,`Vpkgformula []) -> None
    |(_,`String "") -> None
    |e -> Some e
  )
  [priority; name; architecture; number;
  source; sourcenumber; sourceversion; 
  recommends; replaces;
  essential;build_essential]@ l
;;

let set_keep pkg =
  match pkg.essential,Packages.is_on_hold pkg with
  |_,true -> `Keep_version
  |true,_ -> `Keep_package
  |false,_ -> `Keep_none
;;

let add_inst inst pkg =
  if inst then true 
  else Packages.is_installed pkg

let add_extra extras tables pkg =
  add_extra_default extras tables pkg

let tocudf tables ?(options=default_options) ?(inst=false) pkg =
  let bind m f = List.flatten (List.map f m) in
  let encpkgname = CudfAdd.encode pkg.name in
  if not(Option.is_none options.native) then begin
    let native_arch = Option.get options.native in
    let pkgarch =
      if Sources.is_source pkg then
        match options.host with
        |None -> native_arch          (* source package : build deps on the native arch *)
        |Some host_arch  -> host_arch (* source package : build deps on the cross arch *)
      else
        pkg.architecture              (* binary package : dependencies are package specific *)
    in
    let _name = 
      (* if the package is a source package the name does not need an
       * architecture annotation. Nobody depends on it *)
      if Sources.is_source pkg then encpkgname
      else add_arch native_arch pkg.name pkgarch
    in
    let _version = get_cudf_version tables (pkg.name,pkg.version)  in
    let _provides = 
      let archlessprovide = (encpkgname,None) in
      let multiarchprovides = 
        match pkg.multiarch with
        |(`No|`Same) ->
           (* pkgarch provides *)
             (add_arch_l native_arch pkgarch (loadlp tables pkg.provides))
        |`Foreign ->
           (* packages of same name and version of itself in all archs except its own
              each package this package provides is provided in all arches *)
           bind (native_arch::options.foreign) (function
                |arch when arch = pkgarch ->
                    (add_arch_l native_arch arch (loadlp tables pkg.provides))
                |arch ->
                    (add_arch native_arch pkg.name arch,Some(`Eq,_version)) ::
                      (add_arch_l native_arch arch (loadlp tables pkg.provides))
              )
        |`Allowed ->
           (* archless package and arch: any package *)
           (* all provides as arch: any *)
           (* pkgarch provides *)
           let any = (CudfAdd.encode (pkg.name^":any"),None) in
           let l = 
             bind (loadlp tables pkg.provides) (fun (name, c) ->
               [(CudfAdd.encode (name^":any"), c);
               ((add_arch native_arch name pkgarch),c)]
             )
           in any::l
      in archlessprovide :: multiarchprovides
    in
    let _conflicts = 
      let originalconflicts = pkg.breaks @ pkg.conflicts in
      (* self conflict *)
      let sc = (add_arch native_arch pkg.name pkgarch,None) in
      let multiarchconstraints = 
        match pkg.multiarch with
        |(`No|`Foreign|`Allowed) -> 
            (* conflict with all other packages with differents archs *)
            let mac = (encpkgname,None) in
            [sc; mac] 
        |`Same -> 
            (* conflict with packages of same name but different arch and version*)
            let masc =
              List.filter_map (function
                |arch when arch = pkgarch -> None
                |arch -> Some(add_arch native_arch pkg.name arch,Some(`Neq,_version))
              ) (native_arch::options.foreign)
            in
            sc :: masc 
      in
      let multiarchconflicts =
        let selfconflict = function
	  | ((n,a),None) -> (n = pkg.name) || List.exists (fun ((p,_),_) -> p=n) pkg.provides 
	  | ((n,a),_)    -> (n = pkg.name)
	in
        let realpackage = Util.StringHashtbl.mem tables.unit_table in
        match pkg.multiarch with
        |(`No|`Foreign|`Allowed) -> 
            bind (native_arch::options.foreign) (fun arch ->
                add_arch_l native_arch arch (loadl tables originalconflicts)
              )
        |`Same -> 
            (* XXX : Duplicated conflicts ! *)
            bind (native_arch::options.foreign) (fun arch ->
              let l =
                bind originalconflicts (fun ((n,a),c) ->
                  (*
                  debug "M-A-Same: examining pkg %s, conflicting with package %s (self confl = %b)" pkg.name n (selfconflict ((n,a),c));
                  *)
                  match realpackage n, selfconflict ((n,a),c) with
                  |true,false  -> [((n,a),c)]  (* real conflict *)
                  |true, true  -> []           (* self conflict on real package, drop it *)
                  |false,false ->
                      begin match c with 
                      |None -> [((n,a),None)] (* virtual conflict *)
                      |_ -> []                (* real conflict on non-existent package, drop it *)
                      end
                  |false, true ->             (* a virtual package and a self conflict *)
                      begin
                        debug "M-A-Same: pkg %s has a self-conflict via virtual package: %s" pkg.name n;
                        try
                          List.filter_map (fun pn ->
                            if pn <> pkg.name then begin
                              debug "M-A-Same: adding conflict on real package %s for %s" pn pkg.name; 
                              Some((pn,a),None)
                            end else None
                          ) (SSet.elements !(Util.StringHashtbl.find tables.virtual_table n))
                        with Not_found -> []
                      end
                )
              in
              (*
              debug "M-A-Same : %s produces (Debian) conflicts: %s" pkg.name (Printer.string_of_vpkglist l);
              *)
              let l' = add_arch_l native_arch arch (loadl tables l) in
              (*
              debug "M-A-Same : %s produces (CUDF) conflicts: %s" pkg.name (Cudf_types_pp.string_of_vpkglist l');
              *)
              l'
            )
      in
      multiarchconflicts @ multiarchconstraints
    in
    let _depends = 
      List.map (add_arch_l ~deps:true native_arch pkgarch) 
      (loadll tables (pkg.pre_depends @ pkg.depends))
    in
    (* XXX: if ignore essential for the moment we also ignore keep *)
    let _keep =
      if options.ignore_essential then
        `Keep_none
      else if (pkgarch <> native_arch && pkgarch <> "all") then
        `Keep_none
      else
        set_keep pkg
    in
    { Cudf.default_package with
      Cudf.package = _name ;
      Cudf.version = _version ;
      Cudf.keep = _keep ;
      Cudf.depends = _depends ;
      Cudf.conflicts = _conflicts ;
      Cudf.provides = _provides ;
      Cudf.installed = add_inst inst pkg ;
      Cudf.pkg_extra = add_extra options.extras_opt tables pkg ;
    }
  end else
    (* :any and :native are not yet allowed in the Debian archive because of
     * wanna-build not supporting it but at least :native is required to be
     * removed because of build-essential:native
     * :any and :native can safely be removed as only packages of native
     * architecture are considered here anyways
     * XXX : in the future, dependencies on :$arch will be introduced (for example
     * for building cross compilers) they have to be handled here as well *)
    (* XXX : hack I should look for : and not 3%a . encoding should be done
     * after *)
    let remove_qualifier = function
      | n,v when String.ends_with n "%3anative" -> String.slice ~last:(-9) n, v
      | n,v when String.ends_with n "%3aany" -> String.slice ~last:(-6) n, v
      | n,v -> n, v
    in
    let _depends =
      List.map (List.map remove_qualifier)
      (loadll ~enc:true tables (pkg.pre_depends @ pkg.depends))
    in
    { Cudf.default_package with
      Cudf.package = encpkgname ;
      Cudf.version = get_cudf_version tables (pkg.name,pkg.version) ;
      Cudf.keep = if options.ignore_essential then `Keep_none else set_keep pkg;
      Cudf.depends = _depends;
      Cudf.conflicts = loadlc ~enc:true tables pkg.name (pkg.breaks @ pkg.conflicts) ;
      Cudf.provides = loadlp ~enc:true tables pkg.provides ;
      Cudf.installed = add_inst inst pkg; 
      Cudf.pkg_extra = add_extra options.extras_opt tables pkg ;
    }

let lltocudf = loadll
let ltocudf = loadl

let load_list ?options l =
  let timer = Util.Timer.create "Debian.ToCudf" in
  Util.Timer.start timer;
  let tables = init_tables l in
  let pkglist = List.map (tocudf tables ?options) l in
  clear tables;
  Util.Timer.stop timer pkglist

let load_universe ?options l =
  let timer = Util.Timer.create "Debian.ToCudf" in
  let pkglist = load_list ?options l in
  Util.Timer.start timer;
  let univ = Cudf.load_universe pkglist in
  Util.Timer.stop timer univ
