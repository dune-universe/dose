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
open Algo
module Boilerplate=BoilerplateNoRpm

include Util.Logging(struct let label = __FILE__ end) ;;

module Options = struct
  open OptParse
  let description =
    "Report packages that aren't installable in any futures of a repository"
  let options = OptParser.make ~description

  include Boilerplate.MakeOptions(struct let options = options end)

  let explain = StdOpt.store_true ()
  let checkonly = Boilerplate.pkglist_option ()
  let failure = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let summary = StdOpt.store_true ()
  let dump = StdOpt.store_true ()

  open OptParser
  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;

  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failure" ~help:"Show failure" failure;

  add options ~short_name:'s' ~help:"Print summary of broken packages" summary;

  add options ~long_name:"dump" ~help:"Dump the cudf package list and exit" dump;

  include Boilerplate.MakeDistribOptions(struct let options = options end);;

end

let sync (sn,sv,v) p =
  let cn = CudfAdd.encode ("src/"^sn^"/"^sv) in
  {p with
    Cudf.provides = (cn, Some (`Eq, v))::p.Cudf.provides;
    Cudf.conflicts = (cn, Some (`Neq, v))::p.Cudf.conflicts;
  }
;;

let dummy (sn,sv) pkg number equivs version =
  {Cudf.default_package with
   Cudf.package = pkg.Cudf.package;
   version = version;
   conflicts = [(pkg.Cudf.package, None)];
   provides = pkg.Cudf.provides;
   keep = pkg.Cudf.keep;
   pkg_extra = [
     ("number",`String number);
     ("architecture",`String "dummy");
     ("equivs", `String (String.concat "," equivs));
     ("source", `String sn);
     ("sourcenumber", `String number)
   ]
  }
;;

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

let timer = Util.Timer.create "Solver"

let outdated 
  ?(dump=false) 
  ?(failure=false) 
  ?(explain=false) 
  ?(summary=false)
  ?(checklist=None) 
  ?options repository =

  let worktable = Hashtbl.create 1024 in
  let version_acc = ref [] in
  let constraints_table = Debian.Evolution.constraints repository in
  (* the repository here should contain only the most recent version of each
   * package *)
  let realpackages = Hashtbl.create 1023 in

  let clusters = Debian.Debutil.cluster repository in

  (* for each cluster, I associate to it its discriminants,
   * cluster name and binary version *)
  let cluster_iter (sn,sv) l =
    List.iter (fun (version,realversion,cluster) ->
      List.iter (fun pkg ->
        let pn = pkg.Debian.Packages.name in
        if Hashtbl.mem constraints_table pn then begin
          Hashtbl.add realpackages pn ()
        end
      ) cluster;
      let (versionlist, constr) =
        Debian.Evolution.all_ver_constr constraints_table cluster
      in
      version_acc := versionlist @ !version_acc;
      Hashtbl.add worktable (sn,version) (cluster,versionlist,constr)
    ) l
  in

  Hashtbl.iter cluster_iter clusters;

  (* for each package name, that is not a real package,
   * I create a package with version 1 and I put it in a
   * cluster by itself *)
  Hashtbl.iter (fun name constr ->
    if not(Hashtbl.mem realpackages name) then begin
      let vl = Debian.Evolution.all_versions constr in
      let pkg = {
        Debian.Packages.default_package with 
        Debian.Packages.name = name;
        version = "1";
        } 
      in
      let cluster = [pkg] in
      version_acc := vl @ !version_acc;
      Hashtbl.add worktable (name,"1") (cluster,vl,constr)
    end
  ) constraints_table;

  Hashtbl.clear realpackages;
  let versionlist = Util.list_unique ("1"::!version_acc) in

  info "Total Names: %d" (Hashtbl.length worktable);
  info "Total versions: %d" (List.length versionlist);

  let tables = Debian.Debcudf.init_tables ~step:2 ~versionlist repository in
  let getv v = Debian.Debcudf.get_cudf_version tables ("",v) in
  let pkgset = 
    CudfAdd.to_set (
      Hashtbl.fold (fun (sn,version) (cluster,vl,constr) acc0 ->
        let sync_index = ref 1 in
        let discr = Debian.Evolution.discriminant (evalsel getv) vl constr in
        let acc0 = 
          (* by assumption all packages in a cluster are syncronized *)
          List.fold_left (fun l pkg ->
            let p = Debian.Debcudf.tocudf ?options tables pkg in
            (sync (sn,version,1) p)::l
          ) acc0 cluster
        in
        (* the target version is always greater then all versions in equivs *)
        List.fold_left (fun acc1 (target,equiv) ->
          incr sync_index;
          List.fold_left (fun acc2 pkg ->
            let p = Debian.Debcudf.tocudf tables pkg in
            let pv = p.Cudf.version in

            let target = Debian.Evolution.align pkg.Debian.Packages.version target in
            let newv = version_of_target getv target in
            let number = Debian.Evolution.string_of_range target in
            let equivs = List.map Debian.Evolution.string_of_range equiv in

            if newv > pv then begin
              let d = dummy (sn,version) p number equivs newv in
              if List.length cluster > 1 then
                (sync (sn,version,!sync_index) d)::acc2
              else
                d::acc2
            end else acc2
          ) acc1 cluster
        ) acc0 discr
      ) worktable [] 
    )
  in
  let pkglist = (CudfAdd.Cudf_set.elements pkgset) in
  
  if dump then begin
    Cudf_printer.pp_preamble stdout Debian.Debcudf.preamble;
    print_newline ();
    Cudf_printer.pp_packages stdout (List.sort pkglist);
    exit(0)
  end;
      
  let universe = Cudf.load_universe pkglist in
  let universe_size = Cudf.universe_size universe in
  info "Total future: %d" universe_size;

  Hashtbl.clear worktable;
  Hashtbl.clear constraints_table;

  let checklist =
    if Option.is_none checklist then []
    else
      List.map (fun (p,_,v) ->
        Cudf.lookup_package universe (p,getv v)
      ) (Option.get checklist)
  in

  let pp pkg =
    let p = 
      if String.starts_with pkg.Cudf.package "src/" then
        Printf.sprintf "Source conflict (%s)" pkg.Cudf.package
      else pkg.Cudf.package
    in
    let v = 
      if pkg.Cudf.version > 0 then begin
        if String.starts_with pkg.Cudf.package "src/" then 
          string_of_int pkg.Cudf.version
        else 
          try Cudf.lookup_package_property pkg "number"
          with Not_found ->
            if (pkg.Cudf.version mod 2) = 1 then
              Debian.Debcudf.get_real_version tables 
              (pkg.Cudf.package,pkg.Cudf.version)
            else
              fatal "Real package without Debian Version"
      end
      else "nan"
    in
    let l =
      List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["architecture";"source";"sourcenumber";"equivs"]
    in
    (CudfAdd.decode p,v,l)
  in

  let fmt = Format.std_formatter in
  Format.fprintf fmt "@[<v 1>report:@,";

  let results = Diagnostic.default_result universe_size in
  let callback d = 
    if summary then Diagnostic.collect results d ;
    Diagnostic.fprintf ~pp ~failure ~explain fmt d 
  in

  Util.Timer.start timer;
  let i =
    if checklist <> [] then
      Depsolver.listcheck ~callback ~global_constraints:false universe checklist
    else
      Depsolver.univcheck ~callback ~global_constraints:false universe
  in
  ignore(Util.Timer.stop timer ());

  if failure then Format.fprintf fmt "@]@.";

  Format.fprintf fmt "total-packages: %d@." universe_size;
  Format.fprintf fmt "total-broken: %d@." i;

  if summary then
        Format.fprintf fmt "@[%a@]@." (Diagnostic.pp_summary ~pp ()) results;

  results
;; 

let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  let options = 
    match Option.get (Options.set_options `Deb) with
    |Boilerplate.Deb o -> o
    |_ -> fatal "impossible"
  in

  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress)
    ["Depsolver_int.univcheck";"Depsolver_int.init_solver"] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) ["Solver"];
  Boilerplate.all_quiet (OptParse.Opt.get Options.quiet);

  let checklist = OptParse.Opt.opt Options.checkonly in
  let failure = OptParse.Opt.get Options.failure in
  let explain = OptParse.Opt.get Options.explain in
  let summary = OptParse.Opt.get Options.summary in
  let dump = OptParse.Opt.get Options.dump in

  let archs =
    if options.Debian.Debcudf.native <> "" then
      options.Debian.Debcudf.native :: options.Debian.Debcudf.foreign
    else []
  in
  let packagelist = Debian.Packages.input_raw ~archs args in

  ignore(outdated ~summary ~failure ~explain ~dump ~checklist ~options packagelist)
;;

Boilerplate.if_application
~alternatives:["dose-outdated";"dose3-outdated";"edos-outdated";"deb-outdated"] 
__FILE__ main ;;

