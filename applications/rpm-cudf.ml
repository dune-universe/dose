(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

open ExtLib

open Cudf
open Rpm
open Common

module Options =
struct
  let plain = ref false
  let dump_hdlist = ref false
  let outdir = ref ""
end

let usage = Printf.sprintf "usage: %s [-options] [packages file]" (Sys.argv.(0))
let options =
  [
    ("--plain", Arg.Set Options.plain,
    "Do not preserve debian semantic.  Creates a (possibly) unconsistent cudf document.");
    ("--dump", Arg.Set Options.dump_hdlist, "Dump the hdlist contentes in raw format");
    ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
    ("--outdir", Arg.String (fun l -> Options.outdir := l),
    "Specify the results directory");
  ]

(* ========================================= *)

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let uri = ref "" in
  let _ =
    try Arg.parse options (fun f -> uri := f ) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  let l =
     match Input.parse_uri !uri with
     |("hdlist",(_,_,_,_,file),_) -> begin
       if !Options.dump_hdlist then
         (Rpm.Hdlists.dump Format.std_formatter file ; exit(0))
       else
         Rpm.Hdlists.input_raw [file]
     end
     |("synth",(_,_,_,_,file),_) -> begin
         Rpm.Synthesis.input_raw [file]
     end
     |_ -> failwith "Not supported"
   in

  let pkglist =
    let timer = Util.Timer.create "Rpm-cudf.pkglist" in
    Util.Timer.start timer;
    Ipr.init_tables ~cmp:Rpm.Version.compare l ;
    let pl = List.map Ipr.tocudf l in
    Util.Timer.stop timer pl
  in

  let oc =
    if !Options.outdir <> "" then begin
      let dirname = !Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 777 ;
      open_out (Filename.concat dirname ("res.cudf"))
    end else stdout
  in

  List.iter (fun pkg ->
    Printf.fprintf oc "%s\n" (Cudf_printer.string_of_package pkg)
  ) pkglist
;;

main ();;
