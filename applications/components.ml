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

module PGraph = Defaultgraphs.PackageGraph

module Options =
struct
  open OptParse

  let debug = StdOpt.store_true ()
  let out = StdOpt.store_true ()

  let description = "Create one dot file for connected component of the dependecy graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options                 ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'o' ~long_name:"out" ~help:"Do not print the result on std out" out;
end;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  let dg = (PGraph.dependency_graph universe) in

  let output_ch p =
    if OptParse.Opt.get Options.out then begin
      open_out (Printf.sprintf "%s.dot" p)
    end
    else stdout
  in

  List.iter (fun cc ->
    let l = ref [] in
    PGraph.UG.iter_vertex (fun v -> l := v :: !l) cc;
    let u = Cudf.load_universe !l in
    let g = PGraph.dependency_graph u in
    let maxv = ref (0, ref (List.hd(!l))) in
    PGraph.G.iter_vertex (fun v ->
      let d = PGraph.G.in_degree g v in
      if d > fst(!maxv) then
        maxv := (d, ref v)
    ) g;
    let outch = output_ch (!(snd(!maxv))).package in
    PGraph.D.output_graph outch g;
    if not(OptParse.Opt.is_set Options.out) then close_out outch
    else Printf.printf "\n\n"
  ) (PGraph.connected_components (PGraph.undirect dg))
;;

main ();;
