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
  let dump = StdOpt.str_option ()
  let detrans = StdOpt.store_true ()

  let description = ""
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~long_name:"dot" ~help:"Print the graph in dot format" debug;
  add options ~long_name:"dump" ~help:"Dump the transitive reduction of the strong dependency graph" dump;
  add options ~long_name:"detrans" ~help:"Transitive reduction. Used in conjuction with --dot." detrans;
end

(* ----------------------------------- *)

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Algo.Strongdep.main";"Algo.Strongdep.conj"] in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ~bars:bars () ;
  let (universe,_,_) = Boilerplate.load_universe posargs in
  let sdgraph = Strongdeps.strongdeps_univ universe in
  Defaultgraphs.StrongDepGraph.out
  ~dump:(OptParse.Opt.opt Options.dump)
  ~dot:(OptParse.Opt.get Options.dot)
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
