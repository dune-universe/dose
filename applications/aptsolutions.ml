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

module Options =
  struct
    open OptParse
    let debug = StdOpt.store_true ()
    let source = StdOpt.store_false ()
    let outdir = StdOpt.str_option ()

    let options = OptParser.make ()
    open OptParser

    add options ~short_name:'d' ~long_name:"debug" ~help:"Print various aggregate information" debug;
    add options ~long_name:"outdir" ~help:"Send output to a file" outdir;
    add options ~long_name:"from-cudf" ~help:"do no consider \"number\" property" debug;

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

  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;

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
          Util.print_info "remove %s %s" n v;
          remove := (n,v) :: !remove
      end
      else if Str.string_match re_inst line 0 then begin
          let n = Str.matched_group 1 line in
          let v = Str.matched_group 2 line in
          Util.print_info "install %s %s" n v;
          install := (n,v) :: !install
      end
      else if Str.string_match re_up line 0 then begin
          let n = Str.matched_group 1 line in
          let vold = Str.matched_group 2 line in
          let vnew = Str.matched_group 3 line in
          Util.print_info "upgrade %s %s -> %s" n vold vnew;
          install := (n,vnew) :: !install ;
          remove := (n,vold) :: !remove
      end
    done with IO.No_more_input -> () end;
    (!install,!remove)
  in

  if (List.length install) = 0 && (List.length remove) = 0 then begin
    Printf.eprintf "Empty solution or not a solution\n";
    exit 0
  end;

  let t = Hashtbl.create (List.length universe) in
  List.iter (fun pkg ->
    let n = pkg.Cudf.package in
    let v = 
      if OptParse.Opt.get Options.source then
        string_of_int pkg.Cudf.version
      else
        Cudf.lookup_package_property pkg "number" 
    in
    Hashtbl.add t (n,v) pkg
  ) universe;

  List.iter (fun (n,v) ->
    let pkg = try Hashtbl.find t (n,v) with Not_found -> (Printf.eprintf "%s
    %s\n%!" n v ; assert false ) in
    Hashtbl.replace t (n,v) {pkg with Cudf.installed = false }
  ) remove ;

  List.iter (fun (n,v) ->
    let pkg = try Hashtbl.find t (n,v) with Not_found -> (Printf.eprintf "%s
    %s\n%!" n v ; assert false )in
    Hashtbl.replace t (n,v) {pkg with Cudf.installed = true }
  ) install ;

  let l = Hashtbl.fold (fun k v acc -> if v.Cudf.installed then v::acc else acc) t [] in
  if not (Option.is_none preamble) then
      print_endline (Cudf_printer.string_of_preamble (Option.get preamble));

  print_endline (Cudf_printer.string_of_packages l)
;;
    
main ();;

