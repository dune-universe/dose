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
open Common

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let cudf = StdOpt.store_true ()
  let out = StdOpt.str_option ()

  let description = "Check if there exists a (non optimal) solution for a cudf problem"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~long_name:"verbose" ~help:"Verbose" debug;
  add options                 ~long_name:"out"   ~help:"Output file" out;
  add options ~short_name:'c' ~long_name:"cudf"  ~help:"print the cudf solution (if any)" cudf;
end

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  match posargs with
  |[f] ->
      let (p,l,r) = 
        match Boilerplate.parse_cudf f with
        |(p,l,Some r) -> (p,l,r)
        |_ ->  (Printf.eprintf "Not a void cudf document (missing request)\n" ; exit 1)
      in 
      let r = Depsolver.check_request (p,l,r) in
      if Diagnostic.is_solution r then begin
        if OptParse.Opt.get Options.cudf then
          Diagnostic.print ~success:true ~explain:true Format.std_formatter r
        end
      ;
      if not(Diagnostic.is_solution r && OptParse.Opt.get Options.cudf) then begin
        Printf.printf "Check %s\n" f;
        Diagnostic.print ~failure:true ~explain:true Format.std_formatter r
      end
  |_ -> (Printf.eprintf "Too many arguments\n" ; exit 1)
;;

main () ;;
