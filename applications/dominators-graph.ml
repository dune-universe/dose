(**************************************************************************************)
(*  Copyright (C) 2010 Jaap Boender <boender@pps.jussieu.fr>                     *)
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
open Cudf
open Graph
open Defaultgraphs
open Cudf_types_pp
open CudfAdd

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let tarjan = StdOpt.store_true ()
  let do_compare = StdOpt.store_true ()
  let out_file = StdOpt.str_option ()

  let description = "Compute the dominator graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'t' ~long_name:"tarjan" ~help:"Use Tarjan algorithm" tarjan;
  add options ~short_name:'o' ~long_name:"output" ~help:"Send output to file" out_file;
  add options ~long_name:"compare" ~help:"Compare Tarjan and MANCOOSI graphs" do_compare;
end

module G = StrongDepGraph.G;;
module O = StrongDepGraph.O;;
module C = Components.Make(G);;

(* ----------------------------------- *)

module S = Set.Make(struct type t = string * string let compare = Pervasives.compare end)
module SG = PackageGraph.G
module SO = PackageGraph.O;;
module Dom = Dominators.Make(SG)
module D = PackageGraph.D;;

let is graph pkg =
  SG.fold_pred (fun p s -> Cudf_set.add p s) graph pkg (Cudf_set.singleton pkg)
;;

let scons graph pkg =
  SG.fold_succ (fun p s -> Cudf_set.add p s) graph pkg (Cudf_set.singleton pkg)
;;

let () =
begin
  Common.Util.set_verbosity Common.Util.Summary;
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ();
  let (universe,_,_) = Boilerplate.load_universe posargs in

  Common.Util.Progress.enable "Algo.Strongdep.main";

  if OptParse.Opt.get Options.do_compare then
  begin
    let sd_graph = Strongdeps.strongdeps_univ universe in
    let sd_graph_detrans = Strongdeps.strongdeps_univ ~transitive:false universe in
    let tg = Dom.dominators_tarjan sd_graph_detrans
    and mg = Dom.dominators sd_graph in
    let mt_trad_tbl = Hashtbl.create (SG.nb_vertex mg) in
    (* let tm_trad_tbl = Hashtbl.create (SG.nb_vertex tg) in *)

    (* compare vertices *)
    let comma = Pcre.regexp "," in
    SG.iter_vertex (fun v ->
      if not (SG.mem_vertex mg v) then
        List.iter (fun p ->
          let q = Cudf.lookup_packages universe p in
          List.iter (fun x -> 
            if not (SG.mem_vertex mg x) then
              Printf.eprintf "%s\n" p;
            Hashtbl.add mt_trad_tbl x v
          ) q
        ) (Pcre.split ~rex:comma (string_of_pkgname v.package))
    ) tg;
    (* but first & foremost: edges *)
    SO.transitive_reduction mg;
    SG.iter_edges (fun p q ->
      if p.package <> "START" && not (SG.mem_edge mg p q) then
      begin
        let ps = List.flatten (List.map (lookup_packages universe) (Pcre.split ~rex:comma p.package))
        and qs = List.flatten (List.map (lookup_packages universe) (Pcre.split ~rex:comma q.package)) in
        let ex = List.fold_left (fun acc p' ->
          List.fold_left (fun acc' q' ->
            SG.mem_edge mg p' q' || acc'
          ) acc qs
        ) false ps in
        if not ex then
        begin
          Printf.eprintf "In T but not in M: %s -> %s\n" p.package q.package;
          if SG.mem_vertex mg p && SG.mem_vertex mg q then
          begin
            Printf.eprintf "  ps: %s\n" (String.concat "|" (List.map (fun x -> x.package) ps));
            Printf.eprintf "  qs: %s\n" (String.concat "|" (List.map (fun x -> x.package) qs));
            Printf.eprintf "  isp: %s\n" (String.concat "," (List.map (fun x -> x.package) (Cudf_set.elements (is sd_graph p))));        
            Printf.eprintf "  dfs: %s\n" (String.concat "," (List.map (fun x -> x.package) (Cudf_set.elements (Cudf_set.diff (is sd_graph q) (scons sd_graph p)))));        
          end
          else
          begin
            if not (SG.mem_vertex mg p) then
              Printf.eprintf "  %s not in M\n" (string_of_pkgname p.package);
            if not (SG.mem_vertex mg q) then
              Printf.eprintf "  %s not in M\n" (string_of_pkgname q.package);
          end
        end
      end
    ) tg;
    SG.iter_edges (fun p q ->
      if not (SG.mem_edge tg p q) then
      begin
        let p' = try Hashtbl.find mt_trad_tbl p with Not_found -> p in
        let q' = try Hashtbl.find mt_trad_tbl q with Not_found -> q in
        if p' <> q' && not (SG.mem_edge tg p' q') then
          Printf.eprintf "In M but not in T: %s (%s) (%d) -> %s (%s) (%d)\n" p.package p'.package (SG.in_degree sd_graph p) q.package q'.package (SG.in_degree sd_graph q);
      end
    ) mg;
  end
  else
  begin
    let dom_graph =
      if OptParse.Opt.get Options.tarjan then
        Dom.dominators_tarjan (Strongdeps.strongdeps_univ ~transitive:false universe)
      else
        Dom.dominators (Strongdeps.strongdeps_univ universe) in
    begin
      if OptParse.Opt.is_set Options.out_file then
        D.output_graph (open_out (OptParse.Opt.get Options.out_file)) dom_graph
      else
        D.output_graph stdout dom_graph
    end
  end
end;;
