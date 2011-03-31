(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate, Roberto Di Cosmo                       *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common
open Algo

let predbar = Util.Progress.create "challenged" ;;
let info fmt = Util.make_info "challenged" fmt
let warning fmt = Util.make_warning "challenged" fmt
let debug fmt = Util.make_debug "challenged" fmt
let fatal fmt = Util.make_fatal "challenged" fmt

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"find challenged packages"
  include Boilerplate.MakeOptions(struct let options = options end)

  let checkonly = Boilerplate.pkglist_option ()
  let brokenlist = StdOpt.store_true ()
  open OptParser

  add options ~long_name:"select" ~help:"Check only these package ex. (sn1,sv1),(sn2,sv2)" checkonly;
  add options ~short_name:'b' ~help:"Print the list of broken packages" brokenlist;
end

let exclude pkgset pl =
  let sl = CudfAdd.to_set pl in
  CudfAdd.Cudf_set.elements (CudfAdd.Cudf_set.diff pkgset sl)
;;

let dummy pkg number version =
  {Cudf.default_package with
   Cudf.package = pkg.Cudf.package;
   version = version;
   provides = pkg.Cudf.provides;
   pkg_extra = [("number",`String number)]
  }


let upgrade tables universe migrationlist =
  let getv = Debian.Debcudf.get_cudf_version tables in
  let pkgset = 
    Cudf.fold_packages (fun s p -> 
      CudfAdd.Cudf_set.add p s
    ) CudfAdd.Cudf_set.empty universe 
  in
  let to_add = 
    List.fold_left (fun l ((pkg,_),target) ->
      let orig = getv (pkg.Debian.Packages.name,pkg.Debian.Packages.version) in
      let newv =
        match target with
        |`Eq v -> getv (pkg.Debian.Packages.name,v)
        |`Hi v -> (getv (pkg.Debian.Packages.name,v)) + 1
        |`Lo v |`In (_,v) -> (getv (pkg.Debian.Packages.name,v)) - 1
      in
      let p = Cudf.lookup_package universe (pkg.Debian.Packages.name,orig) in
      let number = Debian.Evolution.string_of_range target in
      (dummy p number newv)::l
    ) [] migrationlist
  in
  let to_remove = 
    List.map (fun ((pkg,_),_) -> 
      let orig = getv (pkg.Debian.Packages.name,pkg.Debian.Packages.version) in
      Cudf.lookup_package universe (pkg.Debian.Packages.name,orig) 
    ) migrationlist 
  in
  let universe_subset = exclude pkgset to_remove in
  Cudf.load_universe (to_add@universe_subset)
;;

let add h k v =
  try
    let l = Hashtbl.find h k in
    l := v::!l
  with Not_found ->
    Hashtbl.add h k (ref [v])
;;

(* repository are real packages, 
 * packagelist are cudf packages, 
 * cluster are real packages,
 * future are cudf packages *)
let challenged ?(verbose=false) ?(clusterlist=None) repository =
  let predmap = Hashtbl.create 1023 in
  
  (* distribution specific *)
  let worktable = Hashtbl.create 1024 in
  let clusters = Debian.Debutil.cluster repository in
  let version_acc = ref [] in
  let constraints_table = Debian.Evolution.constraints repository in
  let cluster_iter (sn,sv) l =
    List.iter (fun (version,cluster) ->
      let filter x =
        match Debian.Version.split version, Debian.Version.split x with
        |(_,v,_,_),(_,w,_,_) -> (Debian.Version.compare v w) <= 0
      in
      (* all packages in this cluster have the same version *)
      (*
      Printf.eprintf "%s %s -> %s %d\n%!" sn sv version (List.length cluster);
      List.iter (fun pkg ->
        Printf.eprintf "%s %s\n%!" pkg.Debian.Packages.name pkg.Debian.Packages.version;
      ) cluster;
      *)
      List.iter (fun (target,equiv) ->
        let migrationlist = Debian.Evolution.migrate cluster target in
        (* Printf.eprintf "target : %s\n%!" (Debian.Evolution.string_of_range target);
        List.iter (fun ((pkg,orig),target) ->
          Printf.eprintf "%s : %s -> (%s) %s\n%!" 
          pkg.Debian.Packages.name pkg.Debian.Packages.version 
          (Debian.Evolution.string_of_range orig)
          (Debian.Evolution.string_of_range target);
        ) migrationlist;
        *)

        let vl = 
          List.fold_left (fun acc -> function
            |(_,(`Hi v|`Lo v|`Eq v)) -> v::acc 
            |(_,`In (v1,v2)) -> v1::v2::acc
          ) [] migrationlist
        in
        version_acc := vl @ !version_acc;
        let aligned_target = Debian.Evolution.align version target in
        Hashtbl.add worktable (cluster,(sn,sv,version),target,aligned_target,equiv) migrationlist
      ) (Debian.Evolution.discriminants ~filter constraints_table cluster)
    ) l
  in

  if Option.is_none clusterlist then
    Hashtbl.iter cluster_iter clusters
  else
    List.iter (fun (sn,sv) ->
      begin try
        let l = Hashtbl.find clusters (sn,sv) in
        cluster_iter (sn,sv) l
      with Not_found -> fatal "cluster %s %s is not correctly specified" sn sv end
    ) (Option.get clusterlist) 
  ;

  (* cudf part *)
  let versionlist = Util.list_unique !version_acc in
  let tables = Debian.Debcudf.init_tables ~step:2 ~versionlist repository in
  let pkglist = List.map (Debian.Debcudf.tocudf tables) repository in
  let universe = Cudf.load_universe pkglist in
  let brokenref = Depsolver.univcheck universe in

  Util.Progress.set_total predbar (Hashtbl.length worktable);

  (* computing *)
  let fmt = Format.std_formatter in
  Format.fprintf fmt "@[<v 1>report:@,";
  Format.fprintf fmt "broken packages reference: %d@," brokenref;
  Hashtbl.iter(fun (cluster,(sn,sv,bv),target,aligned_target,equiv) migrationlist ->
    Util.Progress.progress predbar;
    let future = upgrade tables universe migrationlist in

    Format.fprintf fmt "cluster: %s %s@," sn sv;
    Format.fprintf fmt "sub cluster bin version: %s@," bv;
    Format.fprintf fmt "target: %s @," (Debian.Evolution.string_of_range target);
    Format.fprintf fmt "aligned target: %s @," (Debian.Evolution.string_of_range aligned_target);
    Format.fprintf fmt "equiv: %s@," (String.concat " , " (List.map (Debian.Evolution.string_of_range) equiv));

    let callback d = if verbose then Diagnostic.fprintf ~failure:true ~explain:true fmt d in
    let i = Depsolver.univcheck ~callback future in

    Format.fprintf fmt "broken packages: %d@," (i - brokenref);
    
    Hashtbl.add predmap (cluster,(sn,sv),target) (i - brokenref);
  ) worktable ;
  Format.fprintf fmt "@]@.";

  predmap
;;

let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["challenged"] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];
  let clusterlist = OptParse.Opt.opt Options.checkonly in 
  let verbose = OptParse.Opt.get Options.brokenlist in
  let pred = challenged ~verbose ~clusterlist (Debian.Packages.input_raw args) in 
  Hashtbl.iter (fun (_,(sn,sv),target) broken ->
    Format.printf "upgrading cluster %s %s@." sn sv;
    Format.printf "to target %s@." (Debian.Evolution.string_of_range target);
    Format.printf "breaks %d packages@.@." broken
  ) pred
;;

main ();;

