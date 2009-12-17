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

(* parse apt-get -s output and generated a cudf solution of the problem *)

open ExtLib
open ExtString
open Common

let enable_debug () =
  Util.Progress.enable "Rpm.Parse.Hdlists.parse_822_iter";
  Util.set_verbosity Common.Util.Summary
;;

module Options =
  struct
    open OptParse
    let debug = StdOpt.store_true ()
    let dump_hdlist = StdOpt.store_true ()
    let outdir = StdOpt.str_option ()

    let options = OptParser.make ()
    open OptParser

    let g = add_group options ~description:"general options" "general" ;;
    let o = add_group options ~description:"output options" "output" ;;

    add options ~group:g ~long_name:"dump" ~help:"Dump the raw hdlist contents" dump_hdlist;
    add options ~group:g ~short_name:'d' ~long_name:"debug" ~help:"Print various aggregate information" debug;

    add options ~group:o ~long_name:"outdir" ~help:"Send output to a file" outdir;

  end

(* ========================================= *)

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let (doc,apt) =
    match OptParse.OptParser.parse_argv Options.options with
    |[] -> (Printf.eprintf "No input file specified" ; exit 1)
    |_::[] -> (Printf.eprintf "a  bit more input" ; exit 1)
    |doc::apt::_ -> (doc,apt)
  in

  if OptParse.Opt.get Options.debug then
    enable_debug ()
  ;

  let (preamble,universe) =
    match Input.parse_uri doc with
    |("cudf",(_,_,_,_,file),_) -> begin
      let p, u, _ = CudfAdd.parse_cudf file in (p,u)
    end
    |(s,_,_) -> failwith (s^" Not supported")
  in

  let (install,remove) =
    let ch = Input.open_file apt in
    let (install,remove) = (ref [] , ref []) in
    (* Remv [old version] *)
    let re_rem = Str.regexp "^Remv \\([^ ]*\\) \\[\\([^]]*\\)\\].*$" in
    (* Inst [old version] (new version).* *)
    let re_inst = Str.regexp "^Inst \\([^ ]*\\) (\\([^ ]*\\) .*).*$" in
    let re_up = Str.regexp "^Inst \\([^ ]*\\) \\[\\([^]]*\\)\\] (\\([^ ]*\\) .*).*$" in
    begin try while true do
      let line = IO.read_line ch in
      if Str.string_match re_rem line 0 then begin
          let n = Str.matched_group 1 line in
          let v = Str.matched_group 2 line in
          (* Printf.eprintf "remove %s %s\n" n v; *)
          remove := (n,v) :: !remove
      end
      else if Str.string_match re_inst line 0 then begin
          let n = Str.matched_group 1 line in
          let v = Str.matched_group 2 line in
          (* Printf.eprintf "install %s %s\n" n v; *)
          install := (n,v) :: !install
      end
      else if Str.string_match re_up line 0 then begin
          let n = Str.matched_group 1 line in
          let vold = Str.matched_group 2 line in
          let vnew = Str.matched_group 3 line in
          (* Printf.eprintf "upgrade %s %s -> %s \n" n vold vnew; *)
          install := (n,vnew) :: !install ;
          remove := (n,vold) :: !remove
      end
    done with IO.No_more_input -> () end;
    (!install,!remove)
  in

  let t = Hashtbl.create (List.length universe) in
  List.iter (fun pkg ->
    let n = pkg.Cudf.package in
    let v = Cudf.lookup_package_property pkg "number" in
    Hashtbl.add t (n,v) pkg
  ) universe;

  List.iter (fun (n,v) ->
    let pkg = Hashtbl.find t (n,v) in
    Hashtbl.replace t (n,v) {pkg with Cudf.installed = false }
  ) remove ;

  List.iter (fun (n,v) ->
    let pkg = Hashtbl.find t (n,v) in
    Hashtbl.replace t (n,v) {pkg with Cudf.installed = true }
  ) install ;

  let l = Hashtbl.fold (fun k v acc -> if v.Cudf.installed then v::acc else acc) t [] in
  if not (Option.is_none preamble) then
      print_endline (Cudf_printer.string_of_preamble (Option.get preamble));

  print_endline (Cudf_printer.string_of_packages l)
;;
    
main ();;

