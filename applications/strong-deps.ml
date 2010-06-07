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
  let dot = StdOpt.store_true ()
  let dump = StdOpt.store_true ()
  let table =  StdOpt.store_true ()
  let detrans = StdOpt.store_true ()
  let restrain = StdOpt.str_option ()
  let prefix = StdOpt.str_option ~default:"" ()

  let description = "Compute the strong dependency graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~long_name:"prefix" ~help:"Prefix output fils with <prefix>" prefix;
  add options ~long_name:"dot" ~help:"Save the strong dependency graph in dot format" dot;
  add options ~long_name:"dump" ~help:"Save the strong dependency graph" dump;
  add options ~long_name:"table" ~help:"Print the table (package,strong,direct,difference)" table;
  add options ~long_name:"detrans" ~help:"Perform the transitive reduction of the graph" detrans;
  add options ~long_name:"restrain" ~help:"Restrain only to the given packages (;-separated)" restrain;
end

(* ----------------------------------- *)

let impactlist graph q =
  Defaultgraphs.PackageGraph.G.fold_pred (fun p acc -> p :: acc ) graph q []

let mk_filename prefix suffix s = if prefix = "" then s^suffix else prefix^suffix

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Algo.Strongdep.main";"Algo.Strongdep.conj"] in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ~bars () ;
  let (universe,_,_) = Boilerplate.load_universe posargs in
  let prefix = OptParse.Opt.get Options.prefix in
  let sdgraph = 
    if OptParse.Opt.is_set Options.restrain then
      let s = OptParse.Opt.get Options.restrain in
      let pkglist =
        let l = Pcre.split ~rex:(Pcre.regexp ";") s in
        List.flatten (List.map (Cudf.lookup_packages universe) l)
      in
      Strongdeps.strongdeps universe pkglist
    else
    Strongdeps.strongdeps_univ universe
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

(*  |[newl;oldl;oldg] when !Options.incr = true ->
      begin
        let oldgraph = Defaultgraphs.StrongDepGraph.load oldg in
        let g = strong_incr (oldgraph,parse oldl) (parse newl) in
        Defaultgraphs.StrongDepGraph.out
        ~dump:!Options.dump ~dot:!Options.dot ~detrans:!Options.detrans
        g
      end
*)

main ();;
