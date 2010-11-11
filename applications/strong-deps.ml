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
  let description = "Compute the strong dependency graph"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let dot = StdOpt.store_true ()
  let dump = StdOpt.store_true ()
  let table =  StdOpt.store_true ()
  let detrans = StdOpt.store_true ()
  let restrain = StdOpt.str_option ()
  let prefix = StdOpt.str_option ~default:"" ()
  let conj_only = StdOpt.store_true ()

  open OptParser
  add options ~long_name:"prefix" ~help:"Prefix output fils with <prefix>" prefix;
  add options ~long_name:"dot" ~help:"Save the strong dependency graph in dot format" dot;
  add options ~long_name:"dump" ~help:"Save the strong dependency graph" dump;
  add options ~long_name:"table" ~help:"Print the table (package,strong,direct,difference)" table;
  add options ~long_name:"detrans" ~help:"Perform the transitive reduction of the graph" detrans;
  add options ~long_name:"restrain" ~help:"Restrain only to the given packages (;-separated)" restrain;
  add options ~long_name:"conj-only" ~help:"Use the conjunctive graph only" conj_only;
end

(* ----------------------------------- *)

let impactlist graph q =
  Defaultgraphs.PackageGraph.G.fold_pred (fun p acc -> p :: acc ) graph q []

let mk_filename prefix suffix s = if prefix = "" then s^suffix else prefix^suffix

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Strongdeps_int.main";"Strongdeps_int.conj"] in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) bars;
  let (universe,_,_) = Boilerplate.load_universe posargs in
  let prefix = OptParse.Opt.get Options.prefix in
  let sdgraph = 
    if OptParse.Opt.is_set Options.restrain then
      let s = OptParse.Opt.get Options.restrain in
      let pkglist =
        let l = Pcre.split ~rex:(Pcre.regexp ";") s in
        List.flatten (List.map (Cudf.lookup_packages universe) l)
      in
      (if OptParse.Opt.get Options.conj_only
      then Strongdeps.conjdeps universe pkglist
      else Strongdeps.strongdeps universe pkglist)
    else
    (if OptParse.Opt.get Options.conj_only
    then Strongdeps.conjdeps_univ universe
    else Strongdeps.strongdeps_univ universe)
  in
  if OptParse.Opt.get Options.table then begin
    let outch = open_out (mk_filename prefix ".table" "data") in
    let depgraph = Defaultgraphs.PackageGraph.dependency_graph universe in
    let l = 
      Defaultgraphs.PackageGraph.G.fold_vertex (fun p l ->
        let strongimpact = List.length (impactlist sdgraph p) in 
        let directimpact = List.length (impactlist depgraph p) in
        (p,strongimpact - directimpact, strongimpact, directimpact) :: l
      ) depgraph []
    in
    List.iter (fun (p,diff,s,d) ->
      let pkg = CudfAdd.print_package p in
      Printf.fprintf outch "%s , %d, %d, %d\n" pkg s d diff
    ) (List.sort ~cmp:(fun (_,x,_,_) (_,y,_,_) -> y - x) l);
    close_out outch
  end
  ;
  let dump = if OptParse.Opt.get Options.dump then Some (mk_filename prefix ".dump" "data") else None in
  let dot = if OptParse.Opt.get Options.dot then Some (mk_filename prefix ".dot" "graph") else None in
  Defaultgraphs.StrongDepGraph.out 
  ~dump ~dot ~detrans:(OptParse.Opt.get Options.detrans)
  sdgraph
;;

main ();;
