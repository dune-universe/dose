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

  let verbose = StdOpt.incr_option ()
  let upgradeonly = StdOpt.store_true ()
  let single = StdOpt.store_true ()

  let description = "Analyse impact of version change on the impact set of packages"
  let options = OptParser.make ~description:description ()

  open OptParser ;;
  add options ~short_name:'v' ~long_name:"verbose" ~help:"Print additional information" verbose;
  add options ~short_name:'u' ~long_name:"upgradeonly" ~help:"Do not analyse version changes corresponding to downgrades" upgradeonly;
  add options ~short_name:'s' ~long_name:"single" ~help:"Do not cluster packages by source" single;
end

(* ----------------------------------- *)


let predbar = Util.Progress.create "Strongpred"
let debug fmt = Util.make_debug "Strongpred" fmt
let info fmt = Util.make_info "Strongpred" fmt
let warning fmt = Util.make_warning "Strongpred" fmt

let exclude pkgset pl = 
  let sl = CudfAdd.to_set pl in
  CudfAdd.Cudf_set.elements (CudfAdd.Cudf_set.diff pkgset sl)
;;

(*
let changed h p =
  try incr (Hashtbl.find h p) 
  with Not_found -> Hashtbl.add h p (ref 1)
;;
let add h k v =
   try let l = Hashtbl.find h k in l := v::!l
   with Not_found -> Hashtbl.add h k (ref [v])
;;
*)
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

(* for each cluster
     for each possible candidate future version V
       for each package P in the cluster
         for each package Q in the impact set of P
           how many of these packages Q are broken by the migration to version V
           ?????
*)
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

  let check results (source,sourceversion) cluster =
    let all_constraints = constraints conv_table cluster in

    if all_constraints = [] then
      info " ignoring cluster %s (= %s) : no version selector mentions it, so IS(p) is invariant."
      source sourceversion
    else

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
        let pn = CudfAdd.string_of_package package in
        let (_,sv) = conv_table.Predictions.from_cudf (package.Cudf.package,version) in
        info "analysing package %s w.r.t version (%s)" pn sv;
        debug "%d -> %d" package.Cudf.version version;

        let isp = try Hashtbl.find impactset_table package with Not_found -> assert false in
        let psels = Predictions.all_constraints conv_table package.Cudf.package in
        let vl = List.map snd psels in
        let pdiscr = keys (Predictions.discriminants psels) in
        debug "for package %s" pn;
        List.iter (fun (rel,v) ->
          debug " (%s %s / %d)" (string_of_relop rel)
          (snd(conv_table.Predictions.from_cudf (package.Cudf.package,v)))
          v
        ) psels;

        if List.length isp <= 0 then
          info " ignoring package %s : it has an empty impact set." pn
        else if package.Cudf.version = version then 
          info " ignoring package %s : same base version" pn
        else if psels = [] then
          info "ignoring package %s : no constraint mentions it, so IS(p) is invariant" pn
        else if package.Cudf.version > version && (OptParse.Opt.get Options.upgradeonly) then
          info " ignoring package %s : version %s represents a downgrade" pn sv
        else if not (List.mem version pdiscr) then
          info " ignoring package %s : %s is not a discriminant" pn sv
        else if CudfAdd.mem_package universe (package.Cudf.package,version) then begin
          info " ignoring package %s : If we migrate to version %s, then all its impact set becomes uninstallable" pn sv;
          add_results results cluster version package isp (* XXX we should mark this differently *)
        end else 
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
            info " Migrating package %s to version %s breaks %d/%d (=%.2f percent) of its Impact set."
            pn sv nbroken sizeisp (float (nbroken * 100)  /. (float sizeisp));
            info " The broken packages in IS(%s) are:" pn;
            List.iter (fun q -> info " - %s" (CudfAdd.string_of_package q)) broken;
            add_results results cluster version package broken
         end else
            info " We can safely migrate package %s to version %s without breaking any dependency" pn sv;
      ) cluster (* for all packages in the cluster P *)
    ) all_discriminants (* for all discriminants in the cluster V *)
  in

  Util.Progress.set_total predbar size;
  let results = Hashtbl.create 1023 in
  if OptParse.Opt.get Options.single then
    Cudf.iter_packages (fun pkg ->
      info "Analysing package %s" (CudfAdd.string_of_package pkg) ;
      check results (pkg.Cudf.package, CudfAdd.string_of_version pkg) [pkg]
    ) universe
  else
    Hashtbl.iter (fun source hv ->
      Hashtbl.iter (fun sourceversion cluster ->
        info "Analysing cluster %s (= %s)" source sourceversion ;
        check results (source,sourceversion) cluster
      ) hv
    ) (Debian.Debutil.group_by_source universe)
  ;
  Util.Progress.reset predbar;
  results
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  (* Boilerplate.enable_bars ["Strongdeps_int.main"; "Strongdeps_int.conj"]; *)
  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  let results = prediction (universe,from_cudf,to_cudf) in
  ()
  (*
  Hashtbl.iter (fun cluster h ->
    Printf.printf "In cluster X\n";
    Hashtbl.iter (fun v {contents = l} ->
      Printf.printf "If we migrate all packages in this cluster to version V\n";
      List.iter (fun (p,broken) ->
        Printf.printf "The package P breaks\n" ;
        List.iter (fun q ->
          Printf.printf "The package Q"
        ) broken
      ) l
    ) h
  ) results
  *)
;;

main();;
