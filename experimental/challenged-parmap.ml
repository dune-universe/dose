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
module Boilerplate = BoilerplateNoRpm

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
  let downgrades = StdOpt.store_true ()
  let cluster = StdOpt.store_true ()
  let ncores = StdOpt.int_option ~default:1 ()
  open OptParser ;;

  add options ~long_name:"select"
    ~help:"Check only these package ex. (sn1,sv1),(sn2,sv2)" checkonly;
  add options ~long_name:"ncores"
    ~help:"Number of cores to use on a multicore" ncores;
  add options ~long_name:"broken" ~short_name:'b' 
    ~help:"Print the list of broken packages" brokenlist;
  add options ~long_name:"downgrade" ~short_name:'d' 
    ~help:"Check package downgrades" downgrades;
  add options ~short_name:'c' 
    ~help:"Print the list of packages in a cluster" cluster;

end

let pkgset u = 
  Cudf.fold_packages (fun s p -> 
    CudfAdd.Cudf_set.add p s
  ) CudfAdd.Cudf_set.empty u

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

let upgrade tables pkgset universe migrationlist =
  let getv v = Debian.Debcudf.get_cudf_version tables ("",v) in
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

let lesser_or_equal getv target v =
  let v1 = version_of_target getv target in
  v1 <= (getv v)
;;

let pp tables pkg =
  let v =
    try Cudf.lookup_package_property pkg "number"
    with Not_found ->
      if (pkg.Cudf.version mod 2) = 1 then
        Debian.Debcudf.get_real_version tables
        (pkg.Cudf.package,pkg.Cudf.version)
      else
        fatal "Real package without Debian Version"
  in
  let l =
    List.filter_map (fun k ->
      try Some(k,Cudf.lookup_package_property pkg k)
      with Not_found -> None
    ) ["architecture";"source";"sourceversion";"equivs"]
  in (pkg.Cudf.package,v,l)

let string_of_relop = function
  |`Eq -> "="
  |`Neq -> "!="
  |`Geq -> ">="
  |`Gt -> ">>"
  |`Leq -> "<="
  |`Lt -> "<<"

(* repository are real packages, 
 * packagelist are cudf packages, 
 * cluster are real packages,
 * future are cudf packages *)
let challenged 
  ?(downgrades=false) 
  ?(broken=false) 
  ?(cluster=false)
  ?(clusterlist=None) 
  repository =
  (* distribution specific *)

  let worktable = ref [] in
  let clusters = Debian.Debutil.cluster repository in
  let version_acc = ref [] in
  let constraints_table = Debian.Evolution.constraints repository in
  let cluster_iter (sn,sv) l =
    List.iter (fun (version,cluster) ->
      let (versionlist, constr) =
        List.fold_left (fun (_vl,_cl) pkg ->
          let pn = pkg.Debian.Packages.name in
          let pv = pkg.Debian.Packages.version in
          let constr = Debian.Evolution.all_constraints constraints_table pn in
          let vl = pv::(Debian.Evolution.all_versions constr) in
          (vl @ _vl,constr @ _cl)
        ) ([],[]) cluster
      in
      let all_epochs = extract_epochs versionlist in
      let all_norm = add_normalize versionlist in
      let versionlist = add_epochs all_epochs all_norm in
      let (versionlist, constr) =
        (Util.list_unique versionlist,Util.list_unique constr) 
      in
    version_acc := versionlist @ !version_acc;

    worktable := ((sn,version),(cluster,List.unique versionlist,List.unique constr)):: !worktable
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
  let pp = pp tables in
  let pkglist = List.map (Debian.Debcudf.tocudf tables) repository in
  let universe = Cudf.load_universe pkglist in
  let pkgset = pkgset universe in

  Util.Progress.set_total predbar (List.length !worktable);

  info "Total Names: %d" (List.length !worktable);
  info "Total versions: %d" (List.length versionlist);

  (* computing *)
  let predmap = 
    Parmap.parmap ~ncores:(OptParse.Opt.get Options.ncores) (fun ((sn,sv),(cluster,vl,constr)) ->
      let timed=Unix.gettimeofday() in
      let predmap' = ref [] in
      debug "source: %s %s" sn sv;
      Util.Progress.progress predbar;
      debug "Versions: %s" (String.concat ";" vl);
      debug "Constraints: %s" (String.concat " ; " (
                               List.map (fun (c,v) -> Printf.sprintf "%s" v) constr
                              )
                              );
      let discr = Debian.Evolution.discriminant ~bottom:true (evalsel getv) vl constr in
      debug "Discriminants: %d" (List.length discr);
      (*
        if cluster then begin
        let pp fmt pkg = 
          let pp_io_property fmt (n, s) = Format.fprintf fmt "%s: %s@," n s in
          Cudf_printer.pp_package_gen pp_io_property fmt pkg
        in
        let pp_list = Diagnostic.pp_list pp in
        let cudf_cluster = 
          List.map (fun pkg -> 
            let (pn,pv) = (pkg.Debian.Packages.name, getv pkg.Debian.Packages.version) in
            Cudf.lookup_package universe (pn,pv) 
          ) cluster
        in
        Format.fprintf fmt "@[<v 1>clusters:@,%a@]@," pp_list cudf_cluster
        end;
        *)
      List.iter (function 
        (* remove this one to show results that are equivalent to do nothing *)
        | (target,equiv) when not(downgrades) && (lesser_or_equal getv target sv) ->
            debug "target: %s" (Debian.Evolution.string_of_range target);
            debug "equiv: %s" (String.concat " , " (
              List.map (Debian.Evolution.string_of_range) equiv
              ));
            debug "ignored"
        | (target,equiv) ->
            debug "Considering target %s" (Debian.Evolution.string_of_range target);
            let migrationlist = Debian.Evolution.migrate cluster target in
            let future = upgrade tables pkgset universe migrationlist in
            debug "target: %s" (Debian.Evolution.string_of_range target);
            debug "equiv: %s" (String.concat " , " (
              List.map (Debian.Evolution.string_of_range) equiv
              ));

            let callback d = 
              let fmt = Format.std_formatter in
              if broken then Diagnostic.fprintf ~pp ~failure:true ~explain:true fmt d 
            in
            if broken then Format.printf "distcheck: @,";

            (* FIXME: in case the cluster is reduced to a single package, we should check only its impact set here *)

            let i = Depsolver.univcheck ~callback future in
            if broken then Format.printf "@.";

            debug "broken: %d" i;
            predmap' :=  (((sn,sv),(target,equiv)),i):: !predmap'
      ) discr;
      Printf.eprintf "<%s, %s>: %f\n" sn sv (Unix.gettimeofday() -. timed);
      !predmap' (* return predmap fragment *)
    ) (Parmap.L !worktable);
  in List.concat predmap
;;

let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["challenged"] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];
  let clusterlist = OptParse.Opt.opt Options.checkonly in 
  let broken = OptParse.Opt.get Options.brokenlist in
  let cluster = OptParse.Opt.get Options.cluster in
  let downgrades = OptParse.Opt.get Options.downgrades in
  let l = (Debian.Packages.input_raw args) in
  let pred = challenged ~downgrades ~broken ~cluster ~clusterlist l in
  List.iter (fun (((sn,sv),(target,equiv)), broken) ->
    Format.printf "cluster: %s %s@." sn sv;
    Format.printf "target: %s@." (Debian.Evolution.string_of_range target);
    Format.printf "equivs: %s@," (String.concat " , " (
      List.map (Debian.Evolution.string_of_range) equiv
    ));
    Format.printf "breaks: %d@." broken;
    Format.printf "---@."
  ) pred
;;

main ();;

