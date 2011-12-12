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
module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse

  let verbose = StdOpt.incr_option ()
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

  let outdir = StdOpt.str_option ~default:"./" ()
  let seed = StdOpt.int_option ~default:0 ()

  let description = "Generate random cudf instance"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'v' ~help:"Print debug information (can be repeated)" verbose;
  add options ~short_name:'n' ~help:"generate n different random documents" documents;
  add options ~short_name:'i' ~long_name:"install" ~help:"install n random packages" install;
  add options ~short_name:'r' ~long_name:"remove" ~help:"remove n random package" remove;
  add options ~short_name:'u' ~long_name:"upgrade" ~help:"upgrade n random package" upgrade;
  add options ~short_name:'k' ~long_name:"keep" ~help:"add keep version to n random packages" keep;
  add options                 ~long_name:"status" ~help:"package status (822)" status;
  add options                 ~long_name:"upgradeAll" ~help:"generate one upgrade all cudf document" upgradeAll;
  add options                 ~long_name:"rstatus" ~help:"add installed to n random packages" rstatus;
  add options                 ~long_name:"instrelop" ~help:"relop probability for install requests" inst_relop;
  add options                 ~long_name:"remrelop" ~help:"relop probability for remove requests" rem_relop;
  add options                 ~long_name:"outdir" ~help:"specify the output directory" outdir;
  add options                 ~long_name:"seed" ~help:"specify the random generator seed" seed;
end

(* -------------------------------- *)

let debug fmt = Util.make_debug __FILE__ fmt
let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt
let fatal fmt = Util.make_fatal __FILE__ fmt

let get_random ?(ver=0.0) pkglist n =
  let a = Array.of_list pkglist in
  let max = (Array.length a) in
  let l = ref [] in
  for i=0 to n - 1 do
    let j = Random.int (max - 1) in
    let pkg = a.(j) in
    if ver = 0.0 then
      l := (pkg.package,None)::!l
    else if ver = 1.0 then
      l := (pkg.package,Some(`Eq, pkg.version))::!l
    else if Random.float(1.0) < ver then begin
      let relop = 
        (* we set 60% the probability for =, and 20% the probability for < > *) 
        let r = Random.float(1.0) in
        if r < 0.6 then `Eq else
        if r >= 0.6 && r < 0.8 then `Lt else
        if r >= 0.8 && r <= 1.0 then `Gt
        else assert false (* not reachable ? *)
      in
      l := (pkg.package,Some(relop,pkg.version))::!l
    end
    else
      l := (pkg.package,None)::!l
  done;
  !l
;;

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
    if Hashtbl.mem h (pkg.package,Some(`Eq, pkg.version)) then f pkg else pkg
  in
  let installed = ref [] in
  let l =
    List.map (fun pkg ->
      pkg >>
      app rstatus_hash (fun p -> { p with installed = true }) >>
      app keep_hash (fun p -> {p with keep = `Keep_version}) >>
      (fun pkg -> if pkg.installed then installed := pkg::!installed ; pkg)
    ) pkglist 
  in
  if (List.length !installed) = 0 then
    warning "No installed packages in this universe";
  (!installed, l, Cudf.load_universe l)
;;

let to_install_random removed p l =
  let l' = 
    List.filter(fun p ->
      try ignore(List.find (fun (q,_) -> p.package = q) removed) ; false
      with Not_found -> true
    ) l
  in
  get_random ~ver:p l' (OptParse.Opt.get Options.install)
;;

let to_upgrade_random removed l =
  let l' = 
    List.filter(fun p ->
      try ignore(List.find (fun (q,_) -> p.package = q) removed) ; false
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
    end 
    else stdout
  in
  Cudf_printer.pp_cudf oc (preamble,universe,request);
  if oc <> stdout then close_out oc
;;

let main () =

  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Random.init (OptParse.Opt.get Options.seed);

  (* raw -> cudf *)
  let (preamble,pkglist) = 
    let extras_properties =
      [("Size", ("size", `Nat (Some 0)));
       ("Installed-Size", ("installedsize", `Nat (Some 0)))]
    in
    let default_preamble =
      let l = List.map snd extras_properties in
      CudfAdd.add_properties Debian.Debcudf.preamble l
    in
    if not(OptParse.Opt.is_set Options.status) && (List.length posargs) > 0 then
      let f = 
        match Boilerplate.filter None [] posargs with
        |(Url.Cudf,[f]) -> Boilerplate.unpack f
        |_ -> (Printf.eprintf "No status provided. I expect a cudf\n" ; exit 1)
      in
      let preamble, pkglist, _ = Boilerplate.parse_cudf f in
      match preamble with
      |None -> (default_preamble,pkglist)
      |Some preamble -> (preamble,pkglist)
    else 
      (* we assume the status file is alwasy in dpkg format ! *)
      let status =
        if OptParse.Opt.is_set Options.status then 
          Boilerplate.read_deb ~filter:Debian.Packages.status_filter
          (OptParse.Opt.get Options.status)
        else []
      in
      let l = 
        match Boilerplate.filter None [] posargs with
        |(Url.Deb, l) ->
            let filelist = List.map Boilerplate.unpack l in
            Debian.Packages.input_raw filelist
        |_ -> (Printf.eprintf "Only deb files are supported\n" ; exit 1)
      in
      let (pkglist,_,_) = Boilerplate.deb_load_list ~extras:extras_properties ~status l in
      (default_preamble, pkglist)
  in

  Printf.printf "Package %d\n%!" (List.length pkglist);
  Printf.printf "Generating %d random documents with\n%!" (OptParse.Opt.get Options.documents);
  Printf.printf "install : %d\n%!" (OptParse.Opt.get Options.install);
  Printf.printf "remove : %d\n%!" (OptParse.Opt.get Options.remove);
  Printf.printf "upgrade : %d\n%!" (OptParse.Opt.get Options.upgrade);
  Printf.printf "and %d upgrade all document\n%!" (if (OptParse.Opt.get Options.upgradeAll) then 1 else 0);
  
  let rp = OptParse.Opt.get Options.rem_relop in
  let ip = OptParse.Opt.get Options.inst_relop in
  for j = 0 to (OptParse.Opt.get Options.documents) - 1 do
    let rec one () = 
      (* we install with 20% probability to add a relop to the request
       * and we remove with 10% probability to add a relop to the request *)
      let (installed,pkglist,universe) = create_pkglist pkglist in
      let request =
        let r = to_remove_random rp installed in
        { request_id = "RAND-CUDF-GENERATOR" ;
          install = to_install_random r ip pkglist ;
          upgrade = to_upgrade_random r installed ;
          remove =  r ;
          req_extra = [] ; }
      in
      if Diagnostic.is_solution (Depsolver.check_request (None,pkglist,request)) then begin 
        Printf.printf "#%d (installed %d) %!" j (List.length installed);
        create_cudf (preamble,universe,request)
      end else (Printf.printf ".%!" ; one () )
    in one ()
  done
  ;
  if (OptParse.Opt.get Options.upgradeAll) then begin
    let (installed,pkglist,universe) = create_pkglist pkglist in
    let request =
      { request_id = "RAND-CUDF-GENERATOR" ;
        install = [] ;
        upgrade = List.map (fun pkg -> (pkg.package,None)) installed ;
        remove =  [] ;
        req_extra = [] ; }
    in
    create_cudf (preamble,universe,request)
  end
;;

main ();;
