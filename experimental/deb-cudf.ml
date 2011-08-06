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

open Debian
open Common
module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse
  let description = "Generate a cudf universe from debian Packages"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let status = StdOpt.str_option ()
  let outfile = StdOpt.str_option ()
  let architecture = StdOpt.str_option ()

  open OptParser
  add options ~short_name:'s' ~long_name:"status" 
  ~help:"package status (822)" status;

  add options ~short_name:'o' ~long_name:"outfile" 
  ~help:"specify the output file prefix" outfile;

  add options ~long_name:"arch" 
  ~help:"Set the default architecture" architecture;
end

let debug fmt = Util.make_debug "Deb-Check" fmt
let info fmt = Util.make_info "Deb-Check" fmt
let warning fmt = Util.make_warning "Deb-Check" fmt
let fatal fmt = Util.make_fatal "Deb-Check" fmt

(* ========================================= *)

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  let default_arch = OptParse.Opt.opt Options.architecture in

  (* raw -> cudf *)
  let (preamble, pkglist, from_cudf) =
    let status =
      if OptParse.Opt.is_set Options.status then
        Boilerplate.read_deb ~filter:Packages.status_filter 
        (OptParse.Opt.get Options.status)
      else []
    in
    info "status %d" (List.length status);
    let l = Debian.Packages.input_raw ~default_arch posargs in
    let (pkglist,from_cudf,_) = Boilerplate.deb_load_list ~status l in
    (Debian.Debcudf.preamble, pkglist, from_cudf)
  in
  let oc =
    if OptParse.Opt.is_set Options.outfile then
      open_out (OptParse.Opt.get Options.outfile)
    else
      stdout
  in

  Cudf_printer.pp_preamble oc preamble;
  Printf.fprintf oc "\n";
  Cudf_printer.pp_packages oc pkglist;

  if oc <> stdout then close_out oc
;;

main ();;
