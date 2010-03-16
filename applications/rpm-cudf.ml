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
open Boilerplate
open Common
open Rpm

module Options =
  struct
    open OptParse
    let debug = StdOpt.store_true ()
    let dump_hdlist = StdOpt.store_true ()
    let outdir = StdOpt.str_option ()

    let options = OptParser.make ()

    open OptParser

    add options ~long_name:"dump" ~help:"Dump the raw hdlist contents" dump_hdlist;
    add options ~short_name:'d' ~long_name:"debug" ~help:"Print various aggregate information" debug;
    add options ~long_name:"outdir" ~help:"Send output to a file" outdir;

  end

(* ========================================= *)

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Rpm.Parse.Hdlists.parse_822_iter"] in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ~bars () ;
  let uri = argv1 posargs in

  let l =
     match Input.parse_uri uri with
     |("hdlist",(_,_,_,_,file),_) -> begin
       if OptParse.Opt.get Options.dump_hdlist then
         (Hdlists.dump Format.std_formatter file ; exit(0))
       else
         Packages.Hdlists.input_raw [file]
     end
     |("synth",(_,_,_,_,file),_) -> begin
         Packages.Synthesis.input_raw [file]
     end
     |_ -> failwith "Not supported"
   in

  let pkglist =
    let timer = Util.Timer.create "Rpm-cudf.pkglist" in
    Util.Timer.start timer;
    let tables = Rpmcudf.init_tables l in
    let pl = List.map (Rpmcudf.tocudf tables) l in
    Rpmcudf.clear tables;
    Util.Timer.stop timer pl
  in

  let oc =
    if OptParse.Opt.is_set Options.outdir then begin
      let dirname = OptParse.Opt.get Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 777 ;
      open_out (Filename.concat dirname ("res.cudf"))
    end else stdout
  in

  List.iter (fun pkg ->
    Printf.fprintf oc "%s\n" (Cudf_printer.string_of_package pkg)
  ) pkglist
;;

main ();;
