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

open Cudf
open ExtLib
open Common

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let documents = StdOpt.int_option ~default:1 ()
  let install = StdOpt.int_option ~default:0 ()
  let upgrade = StdOpt.int_option ~default:0 ()
  let remove = StdOpt.int_option ~default:0 ()
  let keep = StdOpt.int_option ~default:0 ()
  let rstatus = StdOpt.int_option ~default:0 ()
  let relop = StdOpt.float_option ~default:0.3 ()
  let nostatus = StdOpt.store_true ()
  let upgradeAll = StdOpt.store_true ()

  let outdir = StdOpt.str_option ()
  let seed = StdOpt.int_option ~default:0 ()

  let description = "Generate random cudf instance"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options                 ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'n' ~help:"generate n different random documents" documents;
  add options ~short_name:'i' ~long_name:"install" ~help:"install n random packages" install;
  add options ~short_name:'r' ~long_name:"remove" ~help:"remove n random package" remove;
  add options ~short_name:'u' ~long_name:"upgrade" ~help:"upgrade n random package" upgrade;
  add options ~short_name:'k' ~long_name:"keep" ~help:"add keep version to n random packages" keep;
  add options                 ~long_name:"upgradeAll" ~help:"generate one upgrade all cudf document" upgradeAll;
  add options                 ~long_name:"rstatus" ~help:"add installed to n random packages" rstatus;
  add options                 ~long_name:"nostatus" ~help:"do no consider the first argument as a status file" nostatus;
  add options                 ~long_name:"relop" ~help:"relop probability" relop;
  add options                 ~long_name:"outdir" ~help:"specify the output directory" outdir;
  add options                 ~long_name:"seed" ~help:"specify the random generator seed" seed;
end

(* -------------------------------- *)

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
        let r = Random.float(1.0) in
        if r < 0.3 then `Eq else
        if r > 0.3 && r < 0.6 then `Lt else
        if r > 0.6 && r < 1.0 then `Gt else `Eq
      in
      l := (pkg.package,Some(relop,pkg.version))::!l
    end
    else
      l := (pkg.package,None)::!l
  done;
  !l
;;

let extras_properties =
  [("Size", ("size", `Nat (Some 0)));
   ("Installed-Size", ("installedsize", `Nat (Some 0)))]
;;
let extras = List.map fst extras_properties ;;

let preamble =
  let l = List.map snd extras_properties in
  CudfAdd.add_properties Debian.Debcudf.preamble l
;;

let create_pkglist pkglist status =
  let (>>) f g = g f in
  let build_hash n =
    let l = get_random ~ver:1.0 pkglist n in
    let h = Hashtbl.create (2 * n) in
    List.iter (fun pkg -> Hashtbl.add h pkg ()) l;
    h
  in
  let keep_hash = build_hash (OptParse.Opt.get Options.keep) in
  let rstatus_hash = build_hash (OptParse.Opt.get Options.rstatus) in
  let status_hash =
    List.iter (fun pkg ->
      Hashtbl.add rstatus_hash (pkg.package,Some(`Eq,pkg.version)) ()
    ) status
    ;
    rstatus_hash
  in
  let app h f pkg =
    if Hashtbl.mem h (pkg.package,Some(`Eq, pkg.version)) then f pkg else pkg
  in
  let l =
      List.map (fun pkg ->
        pkg >>
        app status_hash (fun p -> { p with installed = true }) >>
        app keep_hash (fun p -> {p with keep = `Keep_version})
      ) pkglist 
  in
  (l, Cudf.load_universe l)
;;

let to_install_random p l =
  get_random ~ver:p l (OptParse.Opt.get Options.install)
;;

let to_upgrade_random l =
  get_random l (OptParse.Opt.get Options.upgrade)
;;

let to_remove_random p l =
  get_random ~ver:p l (OptParse.Opt.get Options.remove)
;; 

let create_cudf universe (to_install,to_upgrade,to_remove) =
  let oc = 
    if (OptParse.Opt.is_set Options.outdir) then begin
      let tmpfile = Filename.temp_file "rand" ".cudf" in
      let dirname = OptParse.Opt.get Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
      let file = (Filename.concat dirname (Filename.basename tmpfile)) in
      Printf.printf "%s\n%!" file ;
      open_out file
    end else stdout
  in
  let request =
    (* XXX request_id should be a unique identifier *)
    { request_id = "RAND-CUDF-GENERATOR" ;
        install = to_install ;
        upgrade = to_upgrade ;
        remove = to_remove ;
        req_extra = [] ;
    }
  in
  Cudf_printer.pp_cudf (Format.formatter_of_out_channel oc) (preamble,universe,request);
  close_out oc
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);

  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  Random.init (OptParse.Opt.get Options.seed);

  let (statusfile, uris) =
    match posargs with
    |[] -> (Printf.eprintf "Missing input" ; exit 1 )
    |[h] when (OptParse.Opt.get Options.nostatus) -> ("",[h])
    |[h] -> (Printf.eprintf "Missing arguments" ; exit 1)
    |h::t when (OptParse.Opt.get Options.nostatus) -> ("",h::t)
    |h::t -> (h,h::t)
  in

  let (status,_,_) =
    if statusfile = "" then
      ([],(fun p -> assert false),(fun (p,v) -> assert false))
    else Boilerplate.load_list [statusfile]
  in
  let (pkglist,_,_) = Boilerplate.load_list ~extras:extras_properties uris in

  Printf.printf "Generating %d random documents with\n%!" (OptParse.Opt.get Options.documents);
  Printf.printf "install : %d\n%!" (OptParse.Opt.get Options.install);
  Printf.printf "remove : %d\n%!" (OptParse.Opt.get Options.remove);
  Printf.printf "upgrade : %d\n%!" (OptParse.Opt.get Options.upgrade);
  Printf.printf "and %d upgrade all document\n%!" (if (OptParse.Opt.get Options.upgradeAll) then 1 else 0);

  for j = 0 to (OptParse.Opt.get Options.documents) do
    let (pkglist,universe) = create_pkglist pkglist status in
    let installed =
      Cudf.fold_packages (fun l pkg -> 
        if pkg.installed then pkg::l else l
      ) [] universe
    in
    let p = OptParse.Opt.get Options.relop in
    let i = to_install_random p pkglist in
    let u = to_upgrade_random installed in
    let r = to_remove_random p installed in
    Printf.printf "%d %!" j;
    create_cudf universe (i,u,r)
  done
  ;
  if (OptParse.Opt.get Options.upgradeAll) then begin
    let (pkglist,universe) = create_pkglist pkglist status in
    let installed =
      Cudf.fold_packages (fun l pkg -> 
        if pkg.installed then pkg::l else l
      ) [] universe
    in
    create_cudf universe ([],List.map (fun pkg -> (pkg.package,None)) installed,[])
  end
;;

main ();;
