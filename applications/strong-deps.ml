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
  let dot = StdOpt.str_option ()
  let dump = StdOpt.str_option ()
  let table =  StdOpt.store_true ()
  let detrans = StdOpt.store_true ()

  let description = "Compute the strong dependency graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~long_name:"dot" ~help:"Save the strong dependency graph in dot format" dot;
  add options ~long_name:"dump" ~help:"Save the strong dependency graph" dump;
  add options ~long_name:"table" ~help:"Print the table (package,strong,direct,difference)" table;
  add options ~long_name:"detrans" ~help:"Perform the transitive reduction of the graph" detrans;
end

(* ----------------------------------- *)

let d_impactlist graph q =
  Defaultgraphs.SyntacticDependencyGraph.G.fold_pred (fun p acc -> p :: acc ) graph q []

let s_impactlist graph q =
  Defaultgraphs.PackageGraph.G.fold_pred (fun p acc -> p :: acc ) graph q []

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Algo.Strongdep.main";"Algo.Strongdep.conj"] in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ~bars:bars () ;
  let (universe,_,_) = Boilerplate.load_universe posargs in
  let sdgraph = Strongdeps.strongdeps_univ universe in
  if OptParse.Opt.get Options.table then begin
    let depgraph = Defaultgraphs.SyntacticDependencyGraph.dependency_graph universe in
    let getp = function
      |Defaultgraphs.SyntacticDependencyGraph.PkgV.Or(p,_) -> p
      |Defaultgraphs.SyntacticDependencyGraph.PkgV.Pkg p -> p
    in
    let l = 
      Defaultgraphs.SyntacticDependencyGraph.G.fold_vertex (fun p l ->
        let strongimpact = List.length (s_impactlist sdgraph (getp p)) in 
        let directimpact = List.length (d_impactlist depgraph p) in
        (getp p,strongimpact - directimpact, strongimpact, directimpact) :: l
      ) depgraph []
    in
    List.iter (fun (p,diff,s,d) ->
      Printf.printf "%s , %d, %d, %d\n" p.Cudf.package s d diff
    ) (List.sort ~cmp:(fun (_,x,_,_) (_,y,_,_) -> y - x) l)
  end
  ;
  Defaultgraphs.StrongDepGraph.out
  ~dump:(OptParse.Opt.opt Options.dump) 
  (* ~dot:(OptParse.Opt.get Options.dot) *)
  ~detrans:(OptParse.Opt.get Options.detrans)
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
