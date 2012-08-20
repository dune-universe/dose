(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
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
open Algo
(* open Cudf *)

module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse
  let description = "Generate random cudf instances from Debian Packages files and cudf files"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let documents = StdOpt.int_option ~default:1 ()
  let install = StdOpt.int_option ~default:0 ()
  let upgrade = StdOpt.int_option ~default:0 ()
  let remove = StdOpt.int_option ~default:0 ()
  let keep = StdOpt.int_option ~default:0 ()
  let rstatus = StdOpt.int_option ~default:0 ()
  let status = StdOpt.str_option ()
  let rem_relop = StdOpt.float_option ~default:0.1 ()
  let inst_relop = StdOpt.float_option ~default:0.2 ()
  let upgradeAll = StdOpt.store_true ()
  let clusterUpgrade = StdOpt.store_true ()

  let outdir = StdOpt.str_option ~default:"./" ()
  let seed = StdOpt.int_option ~default:0 ()

  open OptParser
  add options ~short_name:'n' ~help:"generate n different random documents" documents;
  add options ~short_name:'i' ~long_name:"install" ~help:"install n random packages" install;
  add options ~short_name:'r' ~long_name:"remove" ~help:"remove n random package" remove;
  add options ~short_name:'u' ~long_name:"upgrade" ~help:"upgrade n random package" upgrade;
  add options ~short_name:'k' ~long_name:"keep" ~help:"add keep version to n random packages" keep;
  add options                 ~long_name:"status" ~help:"package status (822 debian format)" status;
  add options                 ~long_name:"all-upgrade" ~help:"generate one upgrade all cudf document" upgradeAll;
  add options                 ~long_name:"cluster-upgrade" ~help:"upgrade package from big clusters" clusterUpgrade;
  add options                 ~long_name:"rstatus" ~help:"add #n packages to the initial status" rstatus;
  add options                 ~long_name:"instrelop" ~help:"relop probability for install requests" inst_relop;
  add options                 ~long_name:"remrelop" ~help:"relop probability for remove requests" rem_relop;
  add options                 ~long_name:"outdir" ~help:"specify the output directory" outdir;
  add options                 ~long_name:"seed" ~help:"specify the random generator seed" seed;
end

(* -------------------------------- *)

include Util.Logging(struct let label = __FILE__ end) ;;

let get_random ?(ver=0.0) pkglist n =
  let a = Array.of_list pkglist in
  let max = (Array.length a) in
  let l = ref [] in
  for i=0 to n - 1 do
    let j = Random.int (max - 1) in
    let pkg = a.(j) in
    if ver = 0.0 then
      l := (pkg.Cudf.package,None)::!l
    else if ver = 1.0 then
      l := (pkg.Cudf.package,Some(`Eq, pkg.Cudf.version))::!l
    else if Random.float(1.0) < ver then begin
      let relop = 
        (* we set 60% the probability for =, and 20% the probability for < > *) 
        let r = Random.float(1.0) in
        if r < 0.6 then `Eq else
        if r >= 0.6 && r < 0.8 then `Lt else
        if r >= 0.8 && r <= 1.0 then `Gt
        else assert false (* not reachable ? *)
      in
      l := (pkg.Cudf.package,Some(relop,pkg.Cudf.version))::!l
    end
    else
      l := (pkg.Cudf.package,None)::!l
  done;
  !l
;;

(* select a package if it is installed and belongs to a cluster
 * that is composed of n or more packages *)
let select_packages_cluster ?(n=4) to_cudf pkglist =
  let th = Debian.Debutil.cluster pkglist in
  List.filter_map (fun pkg ->
    let (name,version) = (pkg.Debian.Packages.name, pkg.Debian.Packages.version) in
    if Debian.Packages.is_installed pkg then
      let (source, sourceversion) = Debian.Debutil.get_source pkg in
      try
        let l = Hashtbl.find th (source, sourceversion) in
        if List.length l = 0 then raise Not_found
        else if List.length l = 1 then
          let (_,_,pl) = List.hd l in
          if List.length pl >= n then 
            Some (name,snd (to_cudf (name,version)))
          else None
        else Some (name,snd(to_cudf (name,version)))
      with Not_found -> begin
        warning "Package %s is not indexed in the source clusters table" pkg.Debian.Packages.name;
        None
      end
    else None
  ) pkglist

let create_pkglist pkglist =
  let (>>) f g = g f in
  let build_hash n =
    let l = get_random ~ver:1.0 pkglist n in
    let h = Hashtbl.create (2 * n) in
    List.iter (fun pkg -> Hashtbl.add h pkg ()) l;
    h
  in
  let keep_hash = build_hash (OptParse.Opt.get Options.keep) in
  let rstatus_hash = build_hash (OptParse.Opt.get Options.rstatus) in
  let app h f pkg =
    if Hashtbl.mem h (pkg.Cudf.package,Some(`Eq, pkg.Cudf.version)) then f pkg else pkg
  in
  let installed = ref [] in
  let l =
    List.map (fun pkg ->
      pkg >>
      app rstatus_hash (fun p -> { p with Cudf.installed = true }) >>
      app keep_hash (fun p -> {p with Cudf.keep = `Keep_version}) >>
      (fun pkg -> if pkg.Cudf.installed then installed := pkg::!installed ; pkg)
    ) pkglist 
  in
  if (List.length !installed) = 0 then
    warning "No installed packages in this universe";
  (!installed, l, Cudf.load_universe l)
;;

let to_install_random removed p l =
  let l' = 
    List.filter(fun p ->
      try ignore(List.find (fun (q,_) -> p.Cudf.package = q) removed) ; false
      with Not_found -> true
    ) l
  in
  get_random ~ver:p l' (OptParse.Opt.get Options.install)
;;

let to_upgrade_random removed l =
  let l' = 
    List.filter(fun p ->
      try ignore(List.find (fun (q,_) -> p.Cudf.package = q) removed) ; false
      with Not_found -> true
    ) l
  in
  get_random l' (OptParse.Opt.get Options.upgrade)
;;

let to_remove_random p l =
  get_random ~ver:p l (OptParse.Opt.get Options.remove)
;; 

let create_cudf (preamble,universe,request)  =
  let oc = 
    if (OptParse.Opt.get Options.documents) > 1 then begin
      let tmpfile = Printf.sprintf "rand%d.cudf" (Random.int 1000) in
      let dirname = OptParse.Opt.get Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
      let file = (Filename.concat dirname (Filename.basename tmpfile)) in
      Printf.printf "%s\n%!" file ;
      open_out file
    end else stdout
  in
  Cudf_printer.pp_cudf oc (preamble,universe,request);
  if oc <> stdout then close_out oc
;;

let deb_load_list options ?(status=[]) dll =
  let pkglist = List.flatten dll in
  let pkglist = if status = [] then pkglist else Debian.Packages.merge status pkglist in
  let tables = Debian.Debcudf.init_tables pkglist in
  let from_cudf (p,i) = (p,Debian.Debcudf.get_real_version tables (p,i)) in
  let to_cudf (p,v) = (p,Debian.Debcudf.get_cudf_version tables (p,v)) in
  let clusterUpgrade = select_packages_cluster to_cudf pkglist in
  let cll =
    List.map (fun l ->
      (* XXX this is stupid and slow *)
      List.map (Debian.Debcudf.tocudf tables ~options) (Debian.Packages.merge status l)
    ) dll
  in
  let preamble = Debian.Debcudf.preamble in
  (preamble,cll,clusterUpgrade,from_cudf,to_cudf)

let deb_parse_input options ?(status=[]) urilist =
  let archs =
    if options.Debian.Debcudf.native <> "" then
      options.Debian.Debcudf.native :: options.Debian.Debcudf.foreign
    else []
  in
  let dll =
    List.map (fun l ->
      let filelist = List.map Boilerplate.unpack l in
      Debian.Packages.input_raw ~archs filelist
    ) urilist
  in
  deb_load_list options ~status dll

let main () =

  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  (* Util.Warning.all_disabled (); *)
  Random.init (OptParse.Opt.get Options.seed);

  let (preamble,pkglist,clusterUpgrade) = 
    (* cudf - contains information about installed packages *)
    if not(OptParse.Opt.is_set Options.status) && (List.length posargs) > 0 then
      let (preamble, pkglist,to_cudf,_) = Boilerplate.parse_input [posargs] in
      (preamble,List.flatten pkglist,[])
    else 
      (* packages list + status *)
      (* we assume the status file is alwasy in dpkg format ! *)
      let status =
        if OptParse.Opt.is_set Options.status then 
          Boilerplate.read_deb ~filter:Debian.Packages.status_filter
          (OptParse.Opt.get Options.status)
        else []
      in
      let (preamble,pkglist,clusterUpgrade,_,_) = 
        let filelist = List.map (List.map Input.parse_uri) [posargs] in
        deb_parse_input Debian.Debcudf.default_options ~status filelist
      in
        (preamble,List.flatten pkglist,clusterUpgrade)
  in

  info "Package %d" (List.length pkglist);
  info "Generating %d random documents with" (OptParse.Opt.get Options.documents);
  info "install : %d" (OptParse.Opt.get Options.install);
  info "remove : %d" (OptParse.Opt.get Options.remove);
  info "upgrade : %d" (OptParse.Opt.get Options.upgrade);
  info "and %d upgrade all document" (if (OptParse.Opt.get Options.upgradeAll) then 1 else 0);
  
  let rp = OptParse.Opt.get Options.rem_relop in
  let ip = OptParse.Opt.get Options.inst_relop in
  let (installed,pkglist,universe) = create_pkglist pkglist in
  for j = 0 to (OptParse.Opt.get Options.documents) - 1 do
    info "install / remove requests";
    let rec one () = 
      (* we install with 20% probability to add a relop to the request
       * and we remove with 10% probability to add a relop to the request *)
      let request =
        let r = to_remove_random rp installed in
        { Cudf.request_id = "RAND-CUDF-GENERATOR" ;
          install = to_install_random r ip pkglist ;
          upgrade = to_upgrade_random r installed ;
          remove =  r ;
          req_extra = [] ; }
      in
      if Diagnostic.is_solution (Depsolver.check_request (None,pkglist,request)) then begin 
        info "#%d (installed %d) %!" j (List.length installed);
        create_cudf (preamble,universe,request)
      end else (Printf.printf ".%!" ; one () )
    in one ()
  done
  ;
  if (OptParse.Opt.get Options.clusterUpgrade) then begin
    info "cluster upgrade request";
    let ul = List.map (fun (n,v) -> Cudf.lookup_package universe (n,v)) clusterUpgrade in
    for j = 0 to (OptParse.Opt.get Options.documents) - 1 do
      let rec one () = 
        let request =
          { Cudf.request_id = "RAND-CUDF-GENERATOR" ;
            install = [];
            remove = [];
            upgrade = to_upgrade_random [] ul;
            req_extra = [] ; }
        in
        if Diagnostic.is_solution (Depsolver.check_request (None,pkglist,request)) then begin 
          info "#%d (installed %d) %!" j (List.length installed);
          create_cudf (preamble,universe,request)
        end else (Printf.printf ".%!" ; one () )
      in one ()
    done
  end
  ;
  if (OptParse.Opt.get Options.upgradeAll) then begin
    info "upgrade all request";
    let request =
      { Cudf.request_id = "RAND-CUDF-GENERATOR" ;
        install = [] ;
        upgrade = List.map (fun pkg -> (pkg.Cudf.package,None)) installed ;
        remove =  [] ;
        req_extra = [] ; }
    in
    create_cudf (preamble,universe,request)
  end
;;

main ();;
