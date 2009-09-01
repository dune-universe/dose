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
open Debian
IFDEF HASDB THEN
open Db
END
open Common

module Deb = Debian.Packages

module Options =
struct
  let installed = ref false
  let status_file = ref ""
  let outdir = ref ""
end

let usage = Printf.sprintf "usage: %s [-options] [packages file]" (Sys.argv.(0))
let options =
  [
    ("--get-selections", Arg.Set Options.installed,
    "Get the installed packages via 'dpkg -l'");
    ("--status", Arg.String (fun l -> Options.status_file := l),
    "Get the installed packages from a file");
    ("--outdir", Arg.String (fun l -> Options.outdir := l),
    "Specify the results directory");
    ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), 
    "Print debug information");
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

  let installed_h = Hashtbl.create 0 in
  let installed =
    let status = 
      if !Options.installed then "/var/lib/dpkg/status"
      else  if !Options.status_file <> "" then !Options.status_file
      else ""
    in
    if status <> "" then
      let l = Deb.input_raw [status] in
      List.iter (fun pkg -> Hashtbl.add installed_h
      (pkg.Deb.name,pkg.Deb.version) ()) l ;
      l
    else []
  in

  let l =
     match Input.parse_uri !uri with
     |(("pgsql"|"sqlite") as dbtype,info,(Some query)) -> 
IFDEF HASDB THEN
       begin
         Backend.init_database dbtype info (Idbr.parse_query query) ;
         Backend.load_selection (`All)
       end
ELSE
       failwith (dbtype ^ "Not supported")
END
     |("deb",(_,_,_,_,file),_) -> begin
       Deb.input_raw [file] 
     end
     |(s,(_,_,_,_,file),_) -> failwith (s ^ " Not supported")
  in

  let progressbar = Util.progress "to cudf" in
  let timer = Util.Timer.create "Deb-cudf.pkglist" in
  let pkglist =
    let ul = if installed <> [] then (List.unique (l @ installed)) else l in
    let total = List.length ul in
    let i = ref 0 in
    begin
      Util.Timer.start timer;
      let tables = Debian.Debcudf.init_tables ul in
      let res = 
        List.map (fun pkg ->
          progressbar (incr i ; !i , total) ; 
          let inst = Hashtbl.mem installed_h
          (pkg.Deb.name,pkg.Deb.version) in
          Debian.Debcudf.tocudf tables ~inst:inst pkg
        ) ul
      in Util.Timer.stop timer res
    end
  in

  let oc =
    if !Options.outdir <> "" then begin
      let dirname = !Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 777 ;
      open_out (Filename.concat dirname ("res.cudf"))
    end else stdout
  in

  Printf.fprintf oc "%s\n" (Cudf_printer.string_of_preamble Debcudf.preamble) ;

  List.iter (fun pkg ->
    Printf.fprintf oc "%s\n" (Cudf_printer.string_of_package pkg)
  ) pkglist
;;

main ();;
