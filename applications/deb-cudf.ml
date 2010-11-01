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

module Options = struct
  open OptParse
  let description = "Generate a cudf universe from debian Packages"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let status = StdOpt.str_option ()
  let outfile = StdOpt.str_option ()

  open OptParser
  add options ~long_name:"status" ~help:"package status (822)" status;
  add options ~long_name:"outfile" ~help:"specify the output file" outfile;
end

(* ========================================= *)

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);

  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);

  (* raw -> cudf *)
  let (preamble,universe) =
    let status =
      if OptParse.Opt.is_set Options.status then
        Boilerplate.read_deb (OptParse.Opt.get Options.status)
      else []
    in
    let l = Debian.Packages.input_raw posargs in
    let (pkglist,_,_) = Boilerplate.deb_load_list ~status l in
    (Debian.Debcudf.preamble, Cudf.load_universe pkglist)
  in
  let oc =
    if OptParse.Opt.is_set Options.outfile then
      let file = OptParse.Opt.get Options.outfile in
      open_out file
    else
      stdout
  in

  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@.%a@." 
    Cudf_printer.pp_preamble preamble
    Cudf_printer.pp_universe universe;

  if oc <> stdout then close_out oc
;;

main ();;
