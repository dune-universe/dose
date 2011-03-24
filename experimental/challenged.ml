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


let upgrade tables universe migrationlist =
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
  Hashtbl.iter (fun (sn,sv) l ->
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
        Hashtbl.add worktable (cluster,(sn,sv),target) migrationlist
      ) (Debian.Evolution.discriminants ~downgrade:sourceversion constraints_table cluster)
    ) l
  ) clusters;

  (* cudf part *)
  let versionlist = Util.list_unique !version_acc in
  let tables = Debian.Debcudf.init_tables ~step:2 ~versionlist repository in
  let pkglist = List.map (Debian.Debcudf.tocudf tables) repository in
  let universe = Cudf.load_universe pkglist in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let brokenref = Depsolver.univcheck universe in

  Util.Progress.set_total predbar (Hashtbl.length worktable);

  (* computing *)
  let univ_size = List.length pkglist in
  Hashtbl.iter(fun (cluster,(sn,sv),target) migrationlist ->
    Util.Progress.progress predbar;
    flush_all ();
    let future = upgrade tables universe migrationlist in
    
    let results = ref [] in
    let callback d = results := d::!results in

    let i = Depsolver.univcheck ~callback future in
    add predmap (cluster,(sn,sv),target) (i,!results)
  ) worktable
  ;

  predmap
;;

let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["challenged"] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];
  let pred = challenged (Debian.Packages.input_raw args) in 
  Hashtbl.iter (fun (_,(sn,sv),target) (broken,results) ->
    Format.printf "upgrading cluster %s %s@." sn sv;
    Format.printf "to target %s@." (Debian.Evolution.string_of_range target);
    Format.printf "breaks %d packages@.@." (i - brokenref)
  ) pred
;;

main ();;

