(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

let info fmt = Util.make_info "mpm-backend" fmt
let warning fmt = Util.make_warning "mpm-backend" fmt
let debug fmt = Util.make_debug "mpm-backend" fmt
let fatal fmt = Util.make_fatal "mpm-backend" fmt

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"all in one backend for mpm"
  include Boilerplate.MakeOptions(struct let options = options end)

  let outfile = StdOpt.str_option ()
  let architecture = StdOpt.str_option ()
  let merge = StdOpt.store_true ()
  let convert = StdOpt.store_true ()
  let check = StdOpt.store_true ()
  let diff = StdOpt.store_true ()

  open OptParser
  add options ~short_name:'o' ~long_name:"outfile"
  ~help:"specify the output file prefix" outfile;

  add options ~long_name:"arch"
  ~help:"Set the default architecture" architecture;

  add options ~long_name:"merge"
  ~help:"merge a status file and a universe in cudf format" merge;

  add options ~long_name:"convert"
  ~help:"create a cudf universe from a status file and Packages file in debian format" convert;

  add options ~long_name:"check"
  ~help:"check if a cudf file is a solution for a cudf request" check;

  add options ~long_name:"diff"
  ~help:"convert the solution for mpm. Args: univ sol" diff;

end

let merge (posargs,outfile) =
  match posargs with
  |[u;s] ->
      let (preamble,pkglist,_) = Boilerplate.parse_cudf u in
      let (_,su,_) = Boilerplate.load_cudf s in 

      let oc = open_out outfile in
      let fmt = Format.formatter_of_out_channel oc in
      if Option.is_some preamble then
        Format.fprintf fmt "%a@." Cudf_printer.pp_preamble (Option.get preamble);
      List.iter (fun pkg ->
        Format.fprintf fmt "%a@." Cudf_printer.pp_package
        begin
          if Cudf.mem_package su (pkg.Cudf.package,pkg.Cudf.version) then
            {pkg with Cudf.installed = true}
          else pkg
        end
      ) pkglist ;
      close_out oc

  |_ -> fatal "You must specify status and a universe (in cudf format)"
;;

let parse_univ f1 =
  match Boilerplate.load_cudf f1 with
  |_,_,None ->
      (Printf.eprintf "file %s is not a valid cudf document\n" f1 ; exit 1)
  |_,u,Some r -> u,r
;;

let check_sol u r s =
  match Cudf_checker.is_solution (u,r) s with
  |false,reasonlist ->
      (List.iter (fun r ->
        Printf.eprintf "%s\n" (Cudf_checker.explain_reason r)
      ) reasonlist;
      false)
  |true,_ -> true
;;

let check posargs = 
  match posargs with
  |[s;u] ->
      let (univ,req) = parse_univ u in
      let (_,sol,_) = Boilerplate.load_cudf s in
      if check_sol univ req sol then ()
      else fatal "%s is not a valid solution. Discarded" s
  |_ -> fatal "You must specify the solution and the universe (in cudf format)"
;;

let convert (posargs,outfile,default_arch) =
  (* raw -> cudf *)
  let (preamble, pkglist, from_cudf) =
    match posargs with
    |s::univlist when (List.length univlist > 1) ->
        let status = Boilerplate.read_deb s in
        let l = Debian.Packages.input_raw ~default_arch univlist in
        let (pkglist,from_cudf,_) = Boilerplate.deb_load_list ~status l in
        (Debian.Debcudf.preamble, pkglist, from_cudf)
    |_ -> fatal "You must specify a status and a Packages file (in debian format)"
  in

  let oc = open_out (outfile ^ ".cudf") in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." Cudf_printer.pp_preamble preamble;
  List.iter (fun pkg ->
    Format.fprintf fmt "%a@." Cudf_printer.pp_package {pkg with Cudf.installed = false} 
  ) pkglist ;
  close_out oc ;

  let oc = open_out (outfile ^ ".status") in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." Cudf_printer.pp_preamble preamble;
  List.iter (fun pkg ->
    if pkg.Cudf.installed then
      Format.fprintf fmt "%a@." Cudf_printer.pp_package pkg
  ) pkglist ;
  close_out oc;

  let oc = open_out (outfile ^ ".map") in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." Boilerplate.pp_versions_table (from_cudf,pkglist);
  close_out oc;
;;

let pp_pkg fmt s =
  let p = CudfAdd.Cudf_set.choose s in
  Format.fprintf fmt "Package: %s\n" p.Cudf.package;
  Format.fprintf fmt "Version: %a\n" CudfAdd.pp_version p;
  try
    let arch = Cudf.lookup_package_property p "architecture" in 
    Format.fprintf fmt "Architecture: %s\n" arch
  with Not_found -> ()
;;
  
let solution posargs =
  let (sol,univ) =
    match posargs with
    |[c;s] ->
        let (_,sol,_) = Boilerplate.load_cudf s in
        let (_,univ,_) = Boilerplate.load_cudf c in
        (sol,univ)
    |_ -> fatal "You must specify a cudf universe a cudf solution"
  in
  let diff = CudfDiff.diff univ sol in
  Hashtbl.iter (fun pkgname s ->
    if CudfAdd.Cudf_set.is_empty s.CudfDiff.unchanged then begin
      let inst = s.CudfDiff.installed in
      let rem = s.CudfDiff.removed in
      match CudfAdd.Cudf_set.is_empty inst, CudfAdd.Cudf_set.is_empty rem with
      |false,true ->
          Format.printf "Install:\n%a@." pp_pkg inst
      |true,false ->
          Format.printf "Remove:\n%a@." pp_pkg rem
      |false,false ->
          Format.printf "Remove:\n%a@." pp_pkg rem;
          Format.printf "Install:\n%a@." pp_pkg inst
      |true,true -> fatal "soldiff fatal error"
    end
  ) diff
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;

  let outfile =
    if OptParse.Opt.is_set Options.outfile then
      OptParse.Opt.get Options.outfile 
    else
      if (OptParse.Opt.get Options.merge) || (OptParse.Opt.get Options.convert) then
        fatal "you must specify the output file"
      else ""
  in

  if OptParse.Opt.get Options.merge then
    merge (posargs,outfile)
  else if OptParse.Opt.get Options.convert then
    let default_arch = OptParse.Opt.opt Options.architecture in
    convert (posargs,outfile,default_arch)
  else if OptParse.Opt.get Options.check then
    check posargs
  else if OptParse.Opt.get Options.diff then
    solution posargs
  else
    fatal "you must specify at least one of --merge --convert or --check"
;;

main ();;

