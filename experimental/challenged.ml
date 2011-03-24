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

let predbar = Util.Progress.create "Challenged" ;;
let info fmt = Util.make_info "challenged" fmt
let warning fmt = Util.make_warning "challenged" fmt
let debug fmt = Util.make_debug "challenged" fmt
let fatal fmt = Util.make_fatal "challenged" fmt

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"find challenged packages"
  include Boilerplate.MakeOptions(struct let options = options end)

  let fail = StdOpt.store_true ()
  open OptParser
  add options ~short_name:'f' ~long_name:"fail" ~help:"exit with a failoure" fail;
end

let exclude pkgset pl =
  let sl = CudfAdd.to_set pl in
  CudfAdd.Cudf_set.elements (CudfAdd.Cudf_set.diff pkgset sl)
;;

let dummy pkg version =
  {Cudf.default_package with
   Cudf.package = pkg.Cudf.package;
   version = version;
   provides = pkg.Cudf.provides;
   pkg_extra = [("number",`String "")]
  }


(* this function modify the Cudf universe (in place ??) by
 * associating a (real) package in cluster to a cudf package 
 * in universe and then migrating this package according to the 
 * target version. The universe is a renumbered cudf universe,
 * the target version is given in term of intervals of real versions *)
let upgrade tables universe migrationlist =
  (* this function maps all "compatible" versions of all
   * packages in cluster to the target version *)
  let pkgset = Cudf.fold_packages (fun s p -> CudfAdd.Cudf_set.add p s) CudfAdd.Cudf_set.empty universe in
  let to_add = 
    List.fold_left (fun l ((pkg,_),target) ->
      let orig = Debian.Debcudf.get_cudf_version tables (pkg.Debian.Packages.name,pkg.Debian.Packages.version) in
      let newv =
        match target with
        |`Eq v -> Debian.Debcudf.get_cudf_version tables (pkg.Debian.Packages.name,v)
        |`Hi v |`Lo v |`In (_,v) ->
            ((Debian.Debcudf.get_cudf_version tables (pkg.Debian.Packages.name,v)) - 1)
      in
      let p = Cudf.lookup_package universe (pkg.Debian.Packages.name,orig) in
      (dummy p newv)::l
    ) [] migrationlist
  in
  let to_remove = 
    List.map (fun ((pkg,_),_) -> 
      let orig = Debian.Debcudf.get_cudf_version tables (pkg.Debian.Packages.name,pkg.Debian.Packages.version) in
      Cudf.lookup_package universe (pkg.Debian.Packages.name,orig) 
    ) migrationlist 
  in
  let universe_subset = exclude pkgset to_remove in
  Cudf.load_universe (to_add@universe_subset)
;;

(* there are four distribution specific functions :
 * cluster : a function that associates a list of packages to a (source,version)
 * discriminants : a function that find all discriminants of a set of packages
 * convert : a function that converts real packages to cudf packages
 * align : a function that move a real version to a target
 * *)

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
let challenged repository =
  let predmap = Hashtbl.create 1023 in
  
  (* distribution specific *)
  let worktable = Hashtbl.create 1024 in
  let clusters = Debian.Debutil.cluster repository in
  let version_acc = ref [] in
  let constraints_table = Debian.Evolution.constraints repository in
  Hashtbl.iter (fun (sourcename,sourceversion) l ->
    List.iter (fun (version,cluster) ->
      List.iter (fun target ->
        let migrationlist = Debian.Evolution.migrate cluster target in
        let vl = 
          List.fold_left (fun acc -> function
            |(_,(`Hi v|`Lo v|`Eq v)) -> v::acc 
            |(_,`In (v1,v2)) -> v1::v2::acc
          ) [] migrationlist
        in
        version_acc := vl @ !version_acc;
        Hashtbl.add worktable (cluster,sourcename,target) migrationlist
      ) (Debian.Evolution.discriminants ~downgrade:sourceversion constraints_table cluster)
    ) l
  ) clusters;

  (* cudf part *)
  let versionlist = Util.list_unique !version_acc in
  let tables = Debian.Debcudf.init_tables ~step:2 ~versionlist repository in
  let pkglist = List.map (Debian.Debcudf.tocudf tables) repository in
  let universe = Cudf.load_universe pkglist in
  (* let from_cudf (p,i) = (p,Debian.Debcudf.get_real_version tables (p,i)) in *)
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let brokenref = Depsolver.univcheck universe in

  Util.Progress.set_total predbar (Hashtbl.length worktable);

  (* computing *)
  Hashtbl.iter(fun (cluster,sourcename,target) migrationlist ->
    (* info "Working on cluster : %s" sourcename; *)
    Util.Progress.progress predbar;
    flush_all ();
    let future = upgrade tables universe migrationlist in
    
    (* let solver = Depsolver.load future in *)
    let i = Depsolver.univcheck future in
    add predmap (cluster,sourcename,target) i

      (*
    List.iter (fun real_p ->
      (* info "real_p %s %s" real_p.Debian.Packages.name real_p.Debian.Packages.version; *)
      List.iter (fun real_q ->
        if not(List.mem real_q cluster) then begin
          (* info "real_q %s %s" real_q.Debian.Packages.name real_q.Debian.Packages.version; *)
          let (q,i) = to_cudf (real_q.Debian.Packages.name,real_q.Debian.Packages.version) in
          (* info "pkg_q %s %d" q i; *)
          let pkg_q = Cudf.lookup_package future (q,i) in
          if not (Diagnostic.is_solution (Depsolver.edos_install solver pkg_q)) then begin
            (*
begin
            Format.printf "upgrading %s %s@." real_p.Debian.Packages.name real_p.Debian.Packages.version;
            Format.printf "to target %s@." (Debian.Evolution.string_of_range target);
            Format.printf "breaks %s %s@." real_q.Debian.Packages.name real_q.Debian.Packages.version;
end;
*)
            add predmap (real_p,target) real_q
          end
        end
      ) repository
    ) cluster
      *)
  ) worktable
  ;

  predmap
;;

let main () =

  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["Challenged"] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];
  let pred = challenged (Debian.Packages.input_raw args) in 
  Hashtbl.iter (fun (_,sourcename,target) broken ->
    Format.printf "upgrading cluster %s @." sourcename;
    Format.printf "to target %s@." (Debian.Evolution.string_of_range target);
    Format.printf "breaks %d packages@.@." (i - brokenref)
  ) pred

;;

main ();;

