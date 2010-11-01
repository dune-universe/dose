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

  let verbose = StdOpt.incr_option ()
  let cudf = StdOpt.store_true ()
  let out = StdOpt.str_option ()

  let description = "Check if there exists a (non optimal) solution for a cudf problem"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~help:"Print information (can be repeated)" verbose;
  add options                 ~long_name:"out"   ~help:"Output file" out;
  add options ~short_name:'c' ~long_name:"cudf"  ~help:"print the cudf solution (if any)" cudf;
end

let pp_solution fmt = function
  |{Diagnostic.result = Diagnostic.Success (f);} ->
      let is = f ~all:true () in
      Format.fprintf fmt "%a" Cudf_printer.pp_packages is
  |_ -> assert false

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);

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
          if not(Option.is_none p) then 
            Format.printf "%a\n" Cudf_printer.pp_preamble (Option.get p);
          Format.printf "%a" pp_solution r
        end
      ;
      if not(Diagnostic.is_solution r && OptParse.Opt.get Options.cudf) then begin
        Printf.printf "Check %s\n" f;
        Diagnostic.printf ~failure:true ~explain:true r
      end
  |_ -> (Printf.eprintf "Too many arguments\n" ; exit 1)
;;

main () ;;
