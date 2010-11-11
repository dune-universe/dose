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

open Cudf
open ExtLib
open Common
open Algo

module Options = struct
  open OptParse
  let description = "Compute Strong Conflicts"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)
end

let debug fmt = Util.make_debug "StrongConflict" fmt
let info fmt = Util.make_info "StrongConflict" fmt
let warning fmt = Util.make_warning "StrongConflict" fmt


(* ----------------------------------- *)

(*
let soundness universe l =
  let solver = Depsolver.load universe in
  List.iter (fun (p,q) ->
    let d = Depsolver.edos_coinstall solver [p;q] in
    match d.Diagnostic.result with
    |Diagnostic.Success _ -> failwith "Unsound"
    |Diagnostic.Failure _ -> ()
  ) l
  ;
;;
*)

let swap (p,q) = if p.Cudf.package < q.Cudf.package then (p,q) else (q,p)

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = [
    "Strongdeps_int.main";"Strongdeps_int.conj";
    "StrongDepGraph.transfrom.edges";"StrongDepGraph.transfrom.vertex";
    "Strongconflicts_int.local"; "Strongconflicts_int.seeding" 
    ]
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) bars;
  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  let universe = Depsolver.trim universe in
  let g = Strongconflicts.strongconflicts universe in

  (* info "Soundness test" ;
  soundness universe l;
  info "done"; *)

  Strongconflicts.CG.iter_edges (fun x y ->
    let (x,y) = swap (x,y) in
    Printf.printf "%s <-> %s\n" (CudfAdd.string_of_package x) (CudfAdd.string_of_package y)
  ) g
  ;
  info "Total strong conflicts %d" (Strongconflicts.CG.nb_edges g)
;;

main ();;
