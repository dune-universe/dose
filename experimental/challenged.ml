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
  open OptParser ;;

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
  let getv v = Debian.Debcudf.get_cudf_version tables ("",v) in
  let pkgset = 
    Cudf.fold_packages (fun s p -> 
      CudfAdd.Cudf_set.add p s
    ) CudfAdd.Cudf_set.empty universe 
  in
  let to_add = 
    List.fold_left (fun l ((pkg,_),target) ->
      let orig = getv pkg.Debian.Packages.version in
      let newv =
        match target with
        |`Eq v -> getv v
        |`Hi v -> (getv v) + 1
        |`Lo v |`In (_,v) -> (getv v) - 1
      in
      let p = Cudf.lookup_package universe (pkg.Debian.Packages.name,orig) in
      let number = Debian.Evolution.string_of_range target in
      (dummy p number newv)::l
    ) [] migrationlist
  in
  let to_remove = 
    List.map (fun ((pkg,_),_) -> 
      let orig = getv pkg.Debian.Packages.version in
      Cudf.lookup_package universe (pkg.Debian.Packages.name,orig) 
    ) migrationlist 
  in
  let universe_subset = exclude pkgset to_remove in
  Cudf.load_universe (to_add@universe_subset)
;;

let add h k v =
  try let l = Hashtbl.find h k in l := v::!l
  with Not_found -> Hashtbl.add h k (ref [v])
;;

let extract_epochs vl =
  List.unique (
    List.fold_left (fun acc v ->
      let (e,_,_,_) = Debian.Version.split v in
      e :: acc
    ) [] vl
  )

let add_epochs el vl =
  List.fold_left (fun acc1 e ->
    List.fold_left (fun acc2 v ->
      match Debian.Version.split v with
      |("",u,r,b) -> (Debian.Version.concat (e,u,r,b))::v::acc2
      |_ -> v::acc2
    ) acc1 vl
  ) [] el

let add_normalize vl =
  List.fold_left (fun acc v ->
    let (e,u,r,b) = Debian.Version.split v in
    (Debian.Version.concat ("",u,r,""))::v::acc
  ) [] vl

let evalsel getv target constr =
  let evalsel v = function
    |(`Eq,w) ->  v = (getv w)
    |(`Geq,w) -> v >= (getv w)
    |(`Leq,w) -> v <= (getv w)
    |(`Gt,w) ->  v > (getv w)
    |(`Lt,w) ->  v < (getv w)
    |(`Neq,w) -> v <> (getv w)
  in
  match target with
  |`Hi v -> evalsel ((getv v) + 1) constr
  |`Lo v -> evalsel ((getv v) - 1) constr
  |`Eq v -> evalsel (getv v) constr
  |`In (v1,v2) -> evalsel ((getv v2) - 1) constr
;;

let version_of_target getv = function
  |`Eq v -> getv v
  |`Hi v -> (getv v) + 1
  |`Lo v |`In (_,v) -> (getv v) - 1
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
    let (versionlist, constr) =
      let clustervl = List.map (fun pkg -> pkg.Debian.Packages.version) cluster in
      List.fold_left (fun (vl,cl) pkg ->
        let pn = pkg.Debian.Packages.name in
        let constr = Debian.Evolution.all_constraints constraints_table pn in
        let vl = clustervl@(Debian.Evolution.all_versions constr) in
        let el = (extract_epochs vl) in
        let tvl = add_normalize vl in
        let versionlist = add_epochs el tvl in
        (versionlist @ vl, constr @ cl)
      ) ([],[]) cluster
    in
    version_acc := versionlist @ !version_acc;
    Hashtbl.add worktable (sn,version) (cluster,List.unique
    versionlist,List.unique constr)
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
  let getv v = Debian.Debcudf.get_cudf_version tables ("",v) in
  let pkglist = List.map (Debian.Debcudf.tocudf tables) repository in
  let universe = Cudf.load_universe pkglist in
  let brokenref = Depsolver.univcheck universe in

  Util.Progress.set_total predbar (Hashtbl.length worktable);

  info "Total Names: %d" (Hashtbl.length worktable);
  info "Total versions: %d" (List.length versionlist);

  (* computing *)
  let fmt = Format.std_formatter in
  Format.fprintf fmt "@[<v 1>report:@,";
  Format.fprintf fmt "broken packages reference: %d@," brokenref;
  Hashtbl.iter (fun (sn,sv) (cluster,vl,constr) ->
    Util.Progress.progress predbar;
    let discr = Debian.Evolution.discriminant (evalsel getv) vl constr in
    Format.fprintf fmt "@[<v 1>source: %s %s@," sn sv;
    if verbose then begin
      Format.fprintf fmt "@[<v 1>clusters:@,";
      List.iter (fun pkg ->
        let (pn,pv) = (pkg.Debian.Packages.name, getv pkg.Debian.Packages.version) in
        let p = Cudf.lookup_package universe (pn,pv) in
        Format.fprintf fmt "%a@," Cudf_printer.pp_package p
      ) cluster;
      Format.fprintf fmt "@]@,";
    end;
    Format.fprintf fmt "@[<v 1>analysis:@,";
    List.iter (fun (target,equiv) ->
      let migrationlist = Debian.Evolution.migrate cluster target in
      let future = upgrade tables universe migrationlist in

      Format.fprintf fmt "target: %s @," (Debian.Evolution.string_of_range target);
      Format.fprintf fmt "equiv: %s@," (String.concat " , " (List.map (Debian.Evolution.string_of_range) equiv));

      let callback d = 
        if verbose then Diagnostic.fprintf ~failure:true ~explain:true fmt d 
      in
      if verbose then
        Format.fprintf fmt "distcheck: @,";

      let i = Depsolver.univcheck ~callback future in
      Format.fprintf fmt "broken: %d@," i;
      Format.fprintf fmt "@,";
      
    ) discr;
    Format.fprintf fmt "@]@]@,---@.";
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

