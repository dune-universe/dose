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
open Debian

let info fmt = Util.make_info "apt-get backend" fmt
let warning fmt = Util.make_warning "apt-get backend" fmt
let debug fmt = Util.make_debug "apt-get backend" fmt
let fatal fmt = Util.make_fatal "apt-get backend" fmt

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"apt-get backend (EDSP v. 0.3)"
  include Boilerplate.MakeOptions(struct let options = options end)

  let outfile = StdOpt.str_option ()
  open OptParser
  add options ~short_name:'o' ~long_name:"outfile" ~help:"specify the output file prefix" outfile;

end

(*
let make_request request = 
  {Cudf.default_request with
    install = request.Edsp.install;
    remove = request.Edsp.remove;
  }
*)

let main () =
  let args = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];

  let file = List.hd args in
  let ch = Input.open_file file in
  let (request,pkglist) = Edsp.input_raw_ch ch in
  let _ = Input.close_ch ch in
  let tables = Debcudf.init_tables pkglist in
  let default_preamble =
    let l = List.map snd Edsp.extras_tocudf in
    CudfAdd.add_properties Debcudf.preamble l
  in
  let cudfpkglist = List.map (Debcudf.tocudf tables ~extras:Edsp.extras_tocudf) pkglist in
  let oc =
    if OptParse.Opt.is_set Options.outfile then
      open_out (OptParse.Opt.get Options.outfile)
    else
      stdout
  in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." Cudf_printer.pp_preamble default_preamble;
  List.iter (Format.fprintf fmt "%a@." Cudf_printer.pp_package) cudfpkglist ;
  if oc <> stdout then close_out oc ; ()
;;

main ();;

