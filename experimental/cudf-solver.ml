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
open Algo
module Boilerplate = BoilerplateNoRpm

module Options = struct
  open OptParse

  let verbose = StdOpt.incr_option ()
  let out = StdOpt.str_option ()
  let explain = StdOpt.store_true ()

  let description = "Check if there exists a (non optimal) solution for a cudf problem"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~help:"Print information (can be repeated)" verbose;
  add options                 ~long_name:"out"   ~help:"Output file" out;
  add options ~short_name:'e' ~long_name:"explain"  ~help:"print the solution or a failure report" explain;
end

include Util.Logging(struct let label = __FILE__ end) ;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  let explain = OptParse.Opt.get Options.explain in

  match posargs with
  |[f] -> begin
      let (p,l,r) = 
        match Boilerplate.load_cudf f with
        |(None,u,Some r) -> (Cudf.default_preamble,u,r)
        |(Some p,u,Some r) -> (p,u,r)
        |_ ->  fatal "Not a void cudf document (missing request)"
      in 
      match Depsolver.check_request ~explain (p,l,r) with
      |Depsolver.Sat (p,u) -> begin
        Printf.printf "Sat %s\n" f;
        if explain then begin 
          if not(Option.is_none p) then 
            Cudf_printer.pp_preamble stdout (Option.get p);
          Cudf_printer.pp_universe stdout u
        end
      end
      |Depsolver.Unsat (Some d) -> begin
        Printf.printf "Unsat %s\n" f;
        Diagnostic.printf ~failure:true ~explain:true d
      end
      |Depsolver.Unsat None -> Printf.printf "Unsat %s\n" f
      |_ -> ()
  end
  |_ -> (Printf.eprintf "Too many arguments\n" ; exit 1)
;;

main () ;;
