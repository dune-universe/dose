(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s): Pietro Abate                                          *)
(*                                                                        *)
(*  Contributor(s):  ADD minor contributors here                          *)
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

let debug fmt = Util.make_debug "Outdated" fmt
let info fmt = Util.make_info "Outdated" fmt
let warning fmt = Util.make_warning "Outdated" fmt
let fatal fmt = Util.make_fatal "Outdated" fmt

module Options = struct
  open OptParse
  let description =
    "Report packages that aren't installable in any futures of a repository"
  let options = OptParser.make ~description

  include Boilerplate.MakeOptions(struct let options = options end)

  let explain = StdOpt.store_true ()
  let architecture = StdOpt.str_option ()
  let checkonly = Boilerplate.pkglist_option ()
  let brokenlist = StdOpt.store_true ()
  let dump = StdOpt.store_true ()

  open OptParser
  add options ~short_name:'a' ~long_name:"architecture" 
  ~help:"Set the default architecture" architecture;

  add options ~long_name:"select" 
  ~help:"Check only these package ex. (sn1,sv1),(sn2,sv2)" checkonly;
  
  add options ~short_name:'b' 
  ~help:"Print the list of broken packages" brokenlist;

  add options ~long_name:"dump"
  ~help:"Dump the cudf package list" dump;


end

let sync (sn,sv,v) p =
  let cn = "src/"^sn^"/"^sv in
  {p with
    Cudf.provides = (cn, Some (`Eq, v))::p.Cudf.provides;
    Cudf.conflicts = (cn, Some (`Neq, v))::p.Cudf.conflicts;
  }
;;

let dummy pkg number version =
  {Cudf.default_package with
   Cudf.package = pkg.Cudf.package;
   version = version;
   conflicts = pkg.Cudf.conflicts;
   provides = pkg.Cudf.provides;
   pkg_extra = [("number",`String number)]
  }
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

let outdated ?(dump=false) ?(verbose=false) ?(clusterlist=None) repository =
  let worktable = Hashtbl.create 1024 in
  let version_acc = ref [] in
  let constraints_table = Debian.Evolution.constraints repository in
  (* the repository here should contain only the most recent version of each
   * package *)
  let realpackages = Hashtbl.create 1023 in

  let clusters = Debian.Debutil.cluster repository in
  (* for each cluste, I associate to it its discriminants,
   * cluster name and binary version *)
  Hashtbl.iter (fun (sn,sv) l ->
    (* Printf.eprintf "sn:%s sv:%s\n" sn sv; *)
    List.iter (fun (version,cluster) ->
    (* Printf.eprintf "bin ver:%s\n len:%d\n\n" version (List.length cluster); *)
    let (versionlist, constr) =
      let clustervl = List.map (fun pkg -> pkg.Debian.Packages.version) cluster in
      List.fold_left (fun (vl,cl) pkg ->
        Hashtbl.add realpackages pkg.Debian.Packages.name version;
        let pn = pkg.Debian.Packages.name in
        let pv = pkg.Debian.Packages.version in
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
  ) clusters;
(* get all epochs and all versions and generate a new version for each epoch *)

  (* for each package name, that is not a real package,
   * I create a package with version 1 and I put it in a
   * cluster by itself *)
  Hashtbl.iter (fun name constr -> match (name,constr) with
    |(name,_) when (Hashtbl.mem realpackages name) -> ()
    |(name,_) ->
        let vl = Debian.Evolution.all_versions constr in
        let pkg = {
          Debian.Packages.default_package with 
          Debian.Packages.name = name;
          version = "1";
          } 
        in
        let cluster = [pkg] in
        version_acc := vl @ !version_acc;
        Hashtbl.add worktable (name,"") (cluster,vl,constr)
  ) constraints_table;

  let versionlist = Util.list_unique ("1"::!version_acc) in

  info "Total Names: %d" (Hashtbl.length worktable);
  info "Total versions: %d" (List.length versionlist);

  let tables = Debian.Debcudf.init_tables ~step:2 ~versionlist repository in
  let duplicates = Hashtbl.create 1023 in
  let getv v = Debian.Debcudf.get_cudf_version tables ("",v) in
  let pkglist = 
    let s = 
      CudfAdd.to_set (
        Hashtbl.fold (fun (sn,version) (cluster,vl,constr) acc0 ->
          let acc0 = 
            List.fold_left (fun l pkg ->
                let p = Debian.Debcudf.tocudf tables pkg in
                (sync (sn,version,1) p)::l
            ) acc0 cluster
          in
          let discr = Debian.Evolution.discriminant (evalsel getv) vl constr in
          let i = ref 1 in
          List.fold_left (fun acc1 (target,equiv) ->
            List.fold_left (fun acc2 target ->
              incr i;
              List.fold_left (fun acc3 pkg ->
                let p = Debian.Debcudf.tocudf tables pkg in
                let pn = pkg.Debian.Packages.name in
                let pv = p.Cudf.version in

                let target = Debian.Evolution.align pkg.Debian.Packages.version target in
                let newv =
                  match target with
                  |`Eq v -> getv v
                  |`Hi v -> (getv v) + 1
                  |`Lo v |`In (_,v) -> (getv v) - 1
                in
                let number = Debian.Evolution.string_of_range target in

                if newv > pv then begin
                  if List.length cluster > 1 then
                    (sync (sn,version,!i) (dummy p number newv))::acc3
                  else
                    (dummy p number newv)::acc3
                end else acc3

              ) acc2 cluster
            ) acc1 (target::equiv)
          ) acc0 discr
        ) worktable []
      )
    in CudfAdd.Cudf_set.elements s
  in

  if dump then
    List.iter (fun pkg ->
      Format.printf "%a@." Cudf_printer.pp_package pkg
    ) pkglist ;

  (* add additional pseudo-source packages *)
  let universe = Cudf.load_universe pkglist in
  Hashtbl.clear worktable;
  Hashtbl.clear duplicates;
  Hashtbl.clear realpackages;
  Hashtbl.clear constraints_table;
  Debian.Debcudf.clear tables;

  let fmt = Format.std_formatter in
  Format.fprintf fmt "@[<v 1>report:@,";
  let callback d = 
    if verbose then 
      Diagnostic.fprintf ~failure:true ~explain:true fmt d 
  in
  let i = Depsolver.univcheck ~callback universe in
  Format.fprintf fmt "total-packages: %d@," (List.length pkglist);
  Format.fprintf fmt "total-broken: %d@," i;
  Format.fprintf fmt "@]@.";
;; 


let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress)
  ["Depsolver_int.univcheck";"Depsolver_int.init_solver";
  "CudfAdd.build_maps";"Mdf.__load"] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];

  let clusterlist = OptParse.Opt.opt Options.checkonly in
  let verbose = OptParse.Opt.get Options.brokenlist in
  let dump = OptParse.Opt.get Options.dump in

  let default_arch = OptParse.Opt.opt Options.architecture in
  let packagelist = Debian.Packages.input_raw ~default_arch args in
  let o = outdated ~verbose ~dump packagelist in
  ()


;;

main ();;

