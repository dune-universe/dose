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
  let cudf = StdOpt.store_true ()
  let critrem = StdOpt.store_true ()
  let critnew = StdOpt.store_true ()
  let critparanoid = StdOpt.store_true ()
  let critchg = StdOpt.store_true ()
  let out = StdOpt.str_option ()

  let description = "Check if there exists a (non optimal) solution for a cudf problem"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'v' ~help:"Print information (can be repeated)" verbose;
  add options                 ~long_name:"out"   ~help:"Output file" out;
  add options ~short_name:'s' ~long_name:"sol"  ~help:"print the cudf solution (if any)" cudf;
  add options ~short_name:'n' ~long_name:"new"  ~help:"specify an optimization criteria" critnew;
  add options ~short_name:'p' ~long_name:"paranoid"  ~help:"specify an optimization criteria" critparanoid;
  add options ~short_name:'r' ~long_name:"rem"  ~help:"specify an optimization criteria" critrem;
  add options ~short_name:'c' ~long_name:"chg"  ~help:"specify an optimization criteria" critchg;
end

include Util.Logging(struct let label = __FILE__ end) ;;

let pp_solution oc = function
  |{Diagnostic.result = Diagnostic.Success (f)} ->
      let is = f ~all:true () in
      Cudf_printer.pp_packages oc is
  |_ -> assert false

let pp_solution_callback (criteria,res) =
  match res with
  |{Diagnostic.result = Diagnostic.Success (f)} ->
      let is = f ~all:true () in
      Printf.eprintf "-----------\n";
      Cudf_printer.pp_packages stderr is;
      Printf.eprintf "Criteria: %s\n" (String.concat "," (List.map string_of_int (Array.to_list criteria)));
      Printf.eprintf "-----------\n"
  |_ -> assert false

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  match posargs with
  |[f] -> begin
      let (p,l,r) = 
        match Boilerplate.load_cudf f with
        |(None,u,Some r) -> (Cudf.default_preamble,u,r)
        |(Some p,u,Some r) -> (p,u,r)
        |_ ->  fatal "Not a void cudf document (missing request)"
      in 
      let r = 
        if OptParse.Opt.get Options.critparanoid then 
          Depsolver.check_request ~callback:pp_solution_callback ~criteria:[Depsolver_int.Rem;Depsolver_int.New] (p,l,r) 
        else if OptParse.Opt.get Options.critrem then 
          Depsolver.check_request ~callback:pp_solution_callback ~criteria:[Depsolver_int.Rem] (p,l,r) 
        else if OptParse.Opt.get Options.critnew then
          Depsolver.check_request ~callback:pp_solution_callback ~criteria:[Depsolver_int.New] (p,l,r) 
        else if OptParse.Opt.get Options.critchg then
          Depsolver.check_request ~callback:pp_solution_callback ~criteria:[Depsolver_int.Chg] (p,l,r) 
        else
          Depsolver.check_request (p,l,r) 
      in
      begin match r with
      |Algo.Depsolver.Error s -> fatal "%s" s
      |Algo.Depsolver.Unsat _ -> fatal "(UNSAT) No Solutions according to the given preferences"
      |Algo.Depsolver.Sat (solpre,soluniv) ->
        if OptParse.Opt.get Options.cudf then
          if not(Option.is_none solpre) then 
            Cudf_printer.pp_preamble stdout (Option.get solpre);
          Cudf_printer.pp_universe stdout soluniv
        end
      end
  |_ -> (Printf.eprintf "Too many arguments\n" ; exit 1)
;;

main () ;;
