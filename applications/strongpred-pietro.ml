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
  let clustered = StdOpt.store_false ()
  let packages = Boilerplate.pkglist_option ()
  let sdgraph = StdOpt.str_option ()

  open OptParser ;;
  add options ~short_name:'u' ~long_name:"upgradeonly" ~help:"Do not analyse version changes corresponding to downgrades" upgradeonly;
  add options ~short_name:'s' ~long_name:"single" ~help:"Do not cluster packages by source" clustered;
  add options ~long_name:"sel" ~help:"Check only a selection of packages / source" packages;
  add options ~long_name:"sdgraph" ~help:"Load the strong dependency graph" sdgraph;
end

let predbar = Util.Progress.create "Strongpred"
let debug fmt = Util.make_debug "Strongpred" fmt
let info fmt = Util.make_info "Strongpred" fmt
let warning fmt = Util.make_warning "Strongpred" fmt

(* ----------------------------------- *)

type answer = Ignore of string | Failure of string | Success of string

type analysis = {
    package : Cudf.package;
    mutable target : (string * string list);
    mutable impactset : int;
    mutable broken : int;
    mutable answer : answer;
    mutable brokenlist : Cudf.package list;
    mutable discriminants : (int, int list) Hashtbl.t
  }

type cluster = {
  version : string ;
  mutable ignore : bool;
  mutable packages : Cudf.package list;
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
  package = Cudf.default_package; 
  target = ("",[]); 
  broken = 0;
  impactset = 0;
  answer = Success(""); 
  brokenlist = [];
  discriminants = Hashtbl.create 0
} 

let pp_package fmt pkg =
  let p = pkg.Cudf.package in
  let v = CudfAdd.string_of_version pkg in
  Format.fprintf fmt "@[%s (= %s)@]" p v
;;

let pp_source fmt pkg =
  let p = Cudf.lookup_package_property pkg "source" in
  let v = Cudf.lookup_package_property pkg "sourceversion" in
  Format.fprintf fmt "@[%s (= %s)@]" p v
;;

let pp_package_stanza source fmt pkg =
  Format.fprintf fmt "@[name: %a@]" pp_package pkg;
  if source then begin
    Format.fprintf fmt "@,@[source: %a@]" pp_source pkg ;
(*    let s = Cudf.lookup_package_property pkg "sourcesize" in
    Format.fprintf fmt "@,@[size: %s@]" s ;
    *)
  end
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
  |Ignore s -> Format.fprintf fmt "ignore"

let pp_info fmt = function
  |Success s -> Format.fprintf fmt "%s" s
  |Failure s -> Format.fprintf fmt "%s" s
  |Ignore  s -> Format.fprintf fmt "%s" s

let pp_analysis fmt analysis =
  let (v,l) = analysis.target in
  Format.fprintf fmt "package: %a@," pp_package analysis.package;
  Format.fprintf fmt "target: \"%s\"@," v;
  let l = Util.list_unique (List.filter ((<>) v) l) in
  if l <> [] then
    Format.fprintf fmt "equiv: \"%s\"@," (String.concat " , " l);
  Format.fprintf fmt "answer: %a@," pp_answer analysis.answer;
  Format.fprintf fmt "info: %a@," pp_info analysis.answer;
  if analysis.brokenlist <> [] then begin
    Format.fprintf fmt "broken: %d@," analysis.broken;
    Format.fprintf fmt "impactset: %d@," analysis.impactset;
    Format.fprintf fmt "@[<v 1>brokenlist:@,%a@]" (pp_list (pp_package_stanza true)) analysis.brokenlist
  end
;;

let pp_cluster fmt cluster =
  Format.fprintf fmt "version: %s@," cluster.version;
  if cluster.packages <> [] then
    Format.fprintf fmt "@[<v 1>packages:@,%a@]@," (pp_list (pp_package_stanza false)) cluster.packages;
  Format.fprintf fmt "ignore: %b@," cluster.ignore;
  if cluster.analysis <> [] then begin
    Format.fprintf fmt "@[<v 1>analysis:@,%a@]" (pp_list pp_analysis) cluster.analysis
  end
;;

let pp_report fmt report =
  let (p,v) = report.source in
  Format.fprintf fmt "source: %s (= %s)@," p v;
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

let prediction sdgraph (universe1,from_cudf,to_cudf) =
  let conv_table = Predictions.renumber (universe1,from_cudf,to_cudf) in

  (* from this point on we work on the renumbered universe *)
  let universe = conv_table.Predictions.universe in 
  let size = Cudf.universe_size universe in

  let graph = 
    if OptParse.Opt.is_set sdgraph then
      let pkglist = Cudf.get_packages universe in
      Defaultgraphs.StrongDepGraph.load pkglist (OptParse.Opt.get sdgraph)
    else
      Strongdeps.strongdeps_univ universe 
  in

  let pkglist = Cudf.get_packages universe in
  let pkgset = CudfAdd.to_set pkglist in

  let check report (source,sourceversion) cluster =
    let subcluster = { default_cluster with version = sourceversion; packages = cluster } in
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
        let new_universe = Cudf.load_universe (migration_list@universe_subset) in
        let solver = Depsolver.load new_universe in

        (* for each package in the cluster, perform analysis *)
        List.iter (fun package -> 
          let isp = try Hashtbl.find impactset_table package with Not_found -> assert false in
          let psels = (Util.memo Predictions.all_constraints conv_table) package.Cudf.package in
          let pdiscr = (Util.memo (Predictions.discriminants (* ~vl:all_discriminants *) )) psels in
          let vl = keys pdiscr in

          let pn = CudfAdd.string_of_package package in
          let sv = snd(conv_table.Predictions.from_cudf (package.Cudf.package,version)) in
          let sl =
            try 
              List.map (fun v -> 
                snd(conv_table.Predictions.from_cudf (package.Cudf.package,v))
              ) (Hashtbl.find pdiscr version)
            with Not_found -> []
          in
          (* here we report the representant of the equivalence class and all
           * elements in it *)
          let report_package = {default_analysis with package = package; target = (sv,sl)} in

          if package.Cudf.version > version && (OptParse.Opt.get Options.upgradeonly) then begin
            (* user request *)
            let s = Printf.sprintf "package %s : version %s represents a downgrade" pn sv in
            report_package.answer <- Ignore(s);
          end else if package.Cudf.version = version then begin
            (* If I replace a package (p,v) with a dummy package for p with the same version,
               nothing can go wrong *) 
            let s = Printf.sprintf "package %s : same base version. No harm done." pn in
            report_package.answer <- Success(s);
          end else if List.length isp <= 0 then begin
            (* nothing to break here *)
            let s = Printf.sprintf "package %s : it has an empty impact set. No harm done." pn in
            report_package.answer <- Success(s);
          end else if psels = [] then begin
            (* If no version of p is explicitly dependend upon, then *)
            (* changing the version of p does not change its impact set *)
            let s = Printf.sprintf "package %s : no constraint mentions it, so IS(p) is invariant" pn in
            report_package.answer <- Success(s);
          end else if Cudf.mem_package universe (package.Cudf.package,version) then begin
            (* prove the following; if (p,v) and (p,w) are in U, and
               q implies (p,v); then q is not installable when (p,w) replaces (p,v) *)
            let s = Printf.sprintf "package %s : If we migrate to version %s, then all its impact set becomes uninstallable" pn sv in
            report_package.answer <- Failure(s);
          end else if not (List.mem version vl) then begin
            (* this means that some other discriminant subsumes this one *)
            let s = Printf.sprintf "package %s : %s is not a discriminant" pn sv in
            report_package.answer <- Ignore(s);
           end else begin
            (* take care of packages q in isp that may no longer be present in the updated universe *)
            let broken = 
              List.fold_left (fun acc q ->
                if Cudf.mem_package new_universe (q.Cudf.package,q.Cudf.version) then
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
              report_package.brokenlist <- broken ;
           end else begin
              let s = Printf.sprintf "We can safely migrate package %s to version %s without breaking any dependency" pn sv in
              report_package.answer <- Success(s);
           end
          end ;
          subcluster.analysis <- report_package :: subcluster.analysis;
        ) cluster (* for all packages in the cluster P *)
      ) all_discriminants (* for all discriminants in the cluster V *)

    end
  in

  Util.Progress.set_total predbar size;
  let fmt = Format.std_formatter in
  if OptParse.Opt.get Options.clustered then
    let source_clusters = Debian.Debutil.group_by_source universe in
    if OptParse.Opt.is_set Options.packages then
      List.iter (fun (source,sourceversion) ->
        try
          let hv = Hashtbl.find source_clusters (source,sourceversion) in
          let report = { default_report with source = (source,sourceversion) } in
          Hashtbl.iter (fun packageversion cluster ->
            check report (source,packageversion) cluster
          ) hv;
          Format.fprintf fmt "@[%a@]@.---@." pp_report report
        with Not_found -> Printf.eprintf "%s (= %s) Not found" source sourceversion
      ) (OptParse.Opt.get Options.packages)
    else
      Hashtbl.iter (fun (source,sourceversion) hv ->
        let report = { default_report with source = (source,sourceversion) } in
        Hashtbl.iter (fun packageversion cluster ->
          check report (source,packageversion) cluster
        ) hv;
        Format.fprintf fmt "@[%a@]@.---@." pp_report report
      ) source_clusters
  else
    if OptParse.Opt.is_set Options.packages then
      List.iter (fun (source,sourceversion) ->
        try
          let (_,v) = conv_table.Predictions.to_cudf (source,sourceversion) in
          let pkg = Cudf.lookup_package universe (source,v) in
          let (p,v) = (pkg.Cudf.package, CudfAdd.string_of_version pkg) in
          let report = { default_report with source = (p,v) } in
          check report (p,v) [pkg];
          Format.fprintf fmt "@[%a@]@.---@." pp_report report
        with Not_found -> Printf.eprintf "%s (= %s) Not found" source sourceversion
      ) (OptParse.Opt.get Options.packages)
    else
      Cudf.iter_packages (fun pkg ->
        let (p,v) = (pkg.Cudf.package, CudfAdd.string_of_version pkg) in
        let report = { default_report with source = (p,v) } in
        check report (p,v) [pkg];
        Format.fprintf fmt "@[%a@]@.---@." pp_report report
      ) universe
  ;
  Util.Progress.reset predbar
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = [
    "Strongpred";"Strongdeps_int.main";"Strongdeps_int.conj";
    "StrongDepGraph.transfrom.edges";"StrongDepGraph.transfrom.vertex"
    ]
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) bars;
  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  prediction Options.sdgraph (universe,from_cudf,to_cudf)
;;

main();;
