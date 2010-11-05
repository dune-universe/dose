(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Roberto Di Cosmo <roberto@dicosmo.org>                         *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Common

module Options = struct
  open OptParse
  let description = "Analyse impact of version change on the impact set of packages"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let upgradeonly = StdOpt.store_true ()
  let single = StdOpt.store_true ()

  open OptParser ;;
  add options ~short_name:'u' ~long_name:"upgradeonly" ~help:"Do not analyse version changes corresponding to downgrades" upgradeonly;
  add options ~short_name:'s' ~long_name:"single" ~help:"Do not cluster packages by source" single;
end

let predbar = Util.Progress.create "Strongpred"
let debug fmt = Util.make_debug "Strongpred" fmt
let info fmt = Util.make_info "Strongpred" fmt
let warning fmt = Util.make_warning "Strongpred" fmt

(* ----------------------------------- *)

type answer = Failure of string | Success of string

type analysis = {
    package : (string * string);
    mutable target : string;
    mutable impactset : int;
    mutable broken : int;
    mutable answer : answer;
    mutable brokenlist : (string * string) list;
  }

type cluster = {
  version : string ;
  mutable ignore : bool;
  mutable packages : (string * string) list;
  mutable analysis : analysis list
}

type report = {
  source : (string * string);
  mutable clusters : cluster list
}

let default_cluster = {
  version = "";
  ignore = false;
  packages = []; 
  analysis = []
}

let default_report = {
  source = ("",""); 
  clusters = []
} 

let default_analysis = {
  package = ("",""); 
  target = ""; 
  broken = 0;
  impactset = 0;
  answer = Success(""); 
  brokenlist = []
} 

let pp_package fmt (p,v) =
  Format.fprintf fmt "@[%s (= %s)@]" p v
;;

let rec pp_list pp fmt = function
  |[h] -> Format.fprintf fmt "@[<v 1>-@,%a@]" pp h
  |h::t ->
      (Format.fprintf fmt "@[<v 1>-@,%a@]@," pp h ;
      pp_list pp fmt t)
  |[] -> ()
;;

let pp_answer fmt = function
  |Success s -> Format.fprintf fmt "success"
  |Failure s -> Format.fprintf fmt "failure"

let pp_analysis fmt analysis =
  Format.fprintf fmt "package: %a@," pp_package analysis.package;
  Format.fprintf fmt "target: \"%s\"@," analysis.target;
  Format.fprintf fmt "answer: %a@," pp_answer analysis.answer;
  if analysis.brokenlist <> [] then begin
    Format.fprintf fmt "broken: %d@," analysis.broken;
    Format.fprintf fmt "impactset: %d@," analysis.impactset;
    Format.fprintf fmt "@[<v 1>brokenlist:@,%a@]" (pp_list pp_package) analysis.brokenlist
  end
;;

let pp_package_item fmt pkg =
  Format.fprintf fmt "@[name: %a@]" pp_package pkg

let pp_cluster fmt cluster =
  Format.fprintf fmt "version: %s@," cluster.version;
  if cluster.packages <> [] then
    Format.fprintf fmt "@[<v 1>packages:@,%a@]@," (pp_list pp_package_item) cluster.packages;
  Format.fprintf fmt "ignore: %b@," cluster.ignore;
  if cluster.analysis <> [] then begin
    Format.fprintf fmt "@[<v 1>analysis:@,%a@]" (pp_list pp_analysis) cluster.analysis
  end
;;

let pp_report fmt report =
  Format.fprintf fmt "source: %a@," pp_package report.source;
  if report.clusters <> [] then
    Format.fprintf fmt "@[<v 1>clusters:@,%a@]" (pp_list pp_cluster) report.clusters

let exclude pkgset pl = 
  let sl = CudfAdd.to_set pl in
  CudfAdd.Cudf_set.elements (CudfAdd.Cudf_set.diff pkgset sl)
;;

let impactset graph pkglist =
  let h = Hashtbl.create (List.length pkglist) in
  List.iter (fun pkg ->
    Hashtbl.add h pkg (Strongdeps.impactset graph pkg)
  ) pkglist ;
  h
;;

(* collect all version constraints associated to all packages in the cluster *)
let constraints conv_table cluster =
  let h = Hashtbl.create 1023 in
  let add h k v = if not(Hashtbl.mem h k) then Hashtbl.add h k v in
  List.iter (fun pkg ->
    List.iter (fun v -> add h v ()) 
    (Predictions.all_constraints conv_table pkg.Cudf.package)
  ) cluster;
  Hashtbl.fold (fun k _ acc -> k::acc) h []
;;

let add_results results cluster version package broken = 
  try 
    let h = Hashtbl.find results cluster in
    begin try
      let l = Hashtbl.find h version in
      l := (package,broken) :: !l
    with Not_found -> 
      Hashtbl.add h version (ref [(package,broken)])
    end
  with Not_found -> begin
    let h = Hashtbl.create 17 in
    Hashtbl.add h version (ref [(package,broken)]);
    Hashtbl.add results cluster h
  end
;;

let string_of_relop = function
  |`Eq -> "="
  |`Neq -> "!="
  |`Geq -> ">="
  |`Gt -> ">"
  |`Leq -> "<="
  |`Lt -> "<"
;;

let keys h = Hashtbl.fold (fun k _ acc -> k::acc) h [] ;;

let prediction (universe1,from_cudf1,to_cudf1) =
  let conv_table = Predictions.renumber (universe1,from_cudf1,to_cudf1) in

  (* from this point on we work on the renumbered universe *)
  let universe = conv_table.Predictions.universe in 
  let size = Cudf.universe_size universe in

  let pkglist = Cudf.get_packages universe in
  let pkgset = CudfAdd.to_set pkglist in
  let graph = Strongdeps.strongdeps_univ universe in

  let check report (source,sourceversion) cluster =
    let subcluster = { default_cluster with version = sourceversion } in
    subcluster.packages <- List.map (fun pkg -> (pkg.Cudf.package, CudfAdd.string_of_version pkg)) cluster ;
    report.clusters <- subcluster :: report.clusters;

    let all_constraints = constraints conv_table cluster in

    if all_constraints = [] then begin
      subcluster.ignore <- true;
      (*
      let s = Printf.sprintf "ignoring sub-cluster %s (= %s) : no version selector mentions it, so IS(p) is invariant."
      source sourceversion in ()
      *)
    end else begin

      (* precompute versions of packages in this cluster *)
      (* all versions in the cluster that appear in a constraint *)
      (* If no version of p is explicitly dependend upon, then *)
      (* changing the version of p does not change its impact set *)
      (* XXX here there is the assumption that all versions are different!!! *)
      (* XXX this is not version agnostic !!! *)
      let all_discriminants = keys (Predictions.discriminants all_constraints) in

      (* precompute impact sets of the cluster *)
      let impactset_table = impactset graph cluster in
      (* computer remove the cluster packages from the universe *)
      let universe_subset = exclude pkgset cluster in

      List.iter (fun version ->
        Util.Progress.progress predbar;
        (* compute a universe with the relevant packages in the cluster moved to version v *)
        let migration_list = Predictions.migrate conv_table version cluster in
        let new_universe = Cudf.load_universe ((* FIXME: make this list unique!!! *) migration_list@universe_subset) in
        let solver = Depsolver.load new_universe in

        (* for each package in the cluster, perform analysis *)
        List.iter (fun package -> 
          let (p,v) = (package.Cudf.package, CudfAdd.string_of_version package) in
          let pn = CudfAdd.string_of_package package in
          let (_,sv) = conv_table.Predictions.from_cudf (package.Cudf.package,version) in
          let report_package = {default_analysis with package = (p,v); target = sv} in
          debug "%d -> %d" package.Cudf.version version;

          let isp = try Hashtbl.find impactset_table package with Not_found -> assert false in
          let psels = (Util.memo Predictions.all_constraints conv_table) package.Cudf.package in
          let pdiscr = (Util.memo (Predictions.discriminants ~vl:all_discriminants)) psels in
          let vl = keys pdiscr in

          if List.length isp <= 0 then begin
            let s = Printf.sprintf "ignoring package %s : it has an empty impact set." pn in
            report_package.answer <- Failure(s);
          end else if package.Cudf.version = version then begin
            let s = Printf.sprintf "ignoring package %s : same base version" pn in
            report_package.answer <- Failure(s);
          end else if psels = [] then begin
            let s = Printf.sprintf "ignoring package %s : no constraint mentions it, so IS(p) is invariant" pn in
            report_package.answer <- Failure(s);
          end else if package.Cudf.version > version && (OptParse.Opt.get Options.upgradeonly) then begin
            let s = Printf.sprintf "ignoring package %s : version %s represents a downgrade" pn sv in
            report_package.answer <- Failure(s);
          end else if not (List.mem version vl) then begin
            let s = Printf.sprintf "ignoring package %s : %s is not a discriminant" pn sv in
            report_package.answer <- Failure(s);
          end else if CudfAdd.mem_package universe (package.Cudf.package,version) then begin
            let s = Printf.sprintf "ignoring package %s : If we migrate to version %s, then all its impact set becomes uninstallable" pn sv in
            report_package.answer <- Failure(s);
          end else begin
            (* take care of packages q in isp that may no longer be present in the updated universe *)
            let broken = 
              List.fold_left (fun acc q ->
                if CudfAdd.mem_package new_universe (q.Cudf.package,q.Cudf.version) then
                  let d = Depsolver.edos_install solver q in
                  if not(Diagnostic.is_solution d) then q::acc else acc
                else acc
              ) [] isp (* for all packages Q in the impact set of P *)
            in
            let nbroken = List.length broken in
            if nbroken <> 0 then begin 
              let sizeisp = List.length isp in
              let s = Printf.sprintf "Migrating package %s to version %s breaks %d/%d (=%.2f percent) of its Impact set."
              pn sv nbroken sizeisp (float (nbroken * 100)  /. (float sizeisp)) in
              report_package.answer <- Failure(s);
              report_package.broken <- nbroken;
              report_package.impactset <- sizeisp;
              report_package.brokenlist <- List.map (fun pkg -> (pkg.Cudf.package, CudfAdd.string_of_version pkg)) broken ;
           end else begin
              let s = Printf.sprintf "We can safely migrate package %s to version %s without breaking any dependency" pn sv in
              report_package.answer <- Success(s);
           end ;
           subcluster.analysis <- report_package :: subcluster.analysis;
          end
        ) cluster (* for all packages in the cluster P *)
      ) all_discriminants (* for all discriminants in the cluster V *)

    end
  in

  Util.Progress.set_total predbar size;
  let fmt = Format.std_formatter in
  Format.fprintf fmt "@[<v 1>report:@,";
  if OptParse.Opt.get Options.single then
    Cudf.iter_packages (fun pkg ->
      let (p,v) = (pkg.Cudf.package, CudfAdd.string_of_version pkg) in
      let report = { default_report with source = (p,v) } in
      check report (p,v) [pkg];
      Format.fprintf fmt "@[<v 1>-@,%a@,@]" pp_report report
    ) universe
  else
    Hashtbl.iter (fun (source,sourceversion) hv ->
      let report = { default_report with source = (source,sourceversion) } in
      Hashtbl.iter (fun packageversion cluster ->
        check report (source,packageversion) cluster
      ) hv;
      Format.fprintf fmt "@[<v 1>-@,%a@]@," pp_report report
    ) (Debian.Debutil.group_by_source universe)
  ;
  Format.fprintf fmt "@]@.";
  Util.Progress.reset predbar
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["Strongpred"];
  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  prediction (universe,from_cudf,to_cudf)
;;

main();;
