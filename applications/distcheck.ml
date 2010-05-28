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

open Debian
open Common

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let successes = StdOpt.store_true ()
  let failures = StdOpt.store_true ()
  let explain = StdOpt.store_true ()
  let xml = StdOpt.str_option ()

  let showall () = (Opt.get successes) && (Opt.get failures)
  let onlyfail () = (Opt.get failures) && not (Opt.get successes)
  let onlysucc () = (Opt.get successes) && not (Opt.get failures)

  let description = "Report the broken packages in a package list"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'e' ~long_name:"explain" ~help:"Explain the results" explain;
  add options ~short_name:'f' ~long_name:"failures" ~help:"Only show failures" failures;
  add options ~short_name:'s' ~long_name:"successes" ~help:"Only show successes" successes;
  add options ~long_name:"xml" ~help:"Output results in XML format" xml;
end

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs =
    let args = OptParse.OptParser.parse_argv Options.options in
    match Filename.basename(Sys.argv.(0)),args with
    |"debcheck",[] -> ["deb://-"]
    |"debcheck",l -> List.map ((^) "deb://") l
    |_,_ -> args
  in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  let (universe,from_cudf,_) = Boilerplate.load_universe posargs in

  let print_package ?(short=false) pkg =
    let (p,v) = from_cudf pkg in
    Printf.sprintf "(%s,%s)" p v
  in

  let result_printer = function
    (* print all *)
    |{Diagnostic.result = Diagnostic.Success (_) } as r when Options.showall () ->
       Diagnostic.print ~pp:print_package stdout r
    |{Diagnostic.result = Diagnostic.Failure (_) } as r when Options.showall () ->
        Diagnostic.print ~pp:print_package ~explain:(OptParse.Opt.get Options.explain) stdout r

    (* print only success - nothing to explain *)
    |{Diagnostic.result = Diagnostic.Success (_) } as r when Options.onlysucc () ->
        Diagnostic.print ~pp:print_package stdout r
    |{Diagnostic.result = Diagnostic.Failure (_) } when Options.onlysucc () -> ()

    (* print only failures *)
    |{Diagnostic.result = Diagnostic.Success (_) } when Options.onlyfail () -> ()
    |{Diagnostic.result = Diagnostic.Failure (_) } as r when Options.onlyfail () -> 
        Diagnostic.print ~pp:print_package ~explain:(OptParse.Opt.get Options.explain) stdout r

    (* nothing *)
    | _ -> ()
  in

  Util.print_info "Solving..." ;
  let timer = Util.Timer.create "Solver" in
  Util.Timer.start timer;
  let i = Depsolver.univcheck ~callback:result_printer universe in
  ignore(Util.Timer.stop timer ());
  Printf.eprintf "Broken Packages: %d\n%!" i
;;

main () ;;
