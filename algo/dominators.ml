(**************************************************************************************)
(*  Copyright (C) 2009-2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                *)
(*                      and Jaap Boender <boender@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009-2010 Mancoosi Project                                          *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open Graph
open ExtLib
open Common

let dombar = Util.Progress.create "Algo.dominators"
let domtimer = Util.Timer.create "Algo.Dominators.dominators"
let tjntimer = Util.Timer.create "Algo.Dominators.tarjan"
let crtimer = Util.Timer.create "Algo.Dominators.cycle_reduction"
let sdtrtimer = Util.Timer.create "Algo.Dominators.sd_transitive_reduction"
let domtrtimer = Util.Timer.create "Algo.Dominators.dom_transitive_reduction"

include Util.Logging(struct let label = __FILE__ end) ;;

module G = Defaultgraphs.PackageGraph.G
module O = Defaultgraphs.GraphOper(G)
module S = Defaultgraphs.PackageGraph.S

let cycle_reduction g =
  let module Hashtbl = CudfAdd.Cudf_hashtbl in
  let module Set = CudfAdd.Cudf_set in
  let visited = Hashtbl.create (G.nb_vertex g) in
  let rec get_cycle res path v =
    match path with
    |[] -> fatal "No cycle in path!"
    |h::t when G.V.equal h v -> (t, res)
    |h::t -> get_cycle (h::res) t v
  in
  let reduce_cycle path v =
    (* Somewhere, v should be in path. This is the cycle. *)
    let (other, c) = get_cycle [] path v in
    let nv = {
      Cudf.default_package with
      Cudf.version = 1;
      Cudf.package = String.concat "/" (List.sort (List.map (fun p -> p.Cudf.package) (v::c))) }
    in
    G.add_vertex g nv;
    let s = CudfAdd.to_set c in
    List.iter (fun p ->
      if G.mem_vertex g p then begin
        G.iter_pred (fun q -> if not (Set.mem q s) then G.add_edge g q nv) g p;
        G.iter_succ (fun q -> if not (Set.mem q s) then G.add_edge g nv q) g p;
        G.remove_vertex g p;
      end;
      Hashtbl.remove visited p
    ) (v::c);
    (other, nv)
  in
  let rec visit path v =
    if G.mem_vertex g v then begin
      Hashtbl.add visited v true;
      G.iter_succ (fun w ->
        try
          if Hashtbl.find visited w then
            let (other, nv) = reduce_cycle (v::path) w in
            visit other nv
        with Not_found -> visit (v::path) w
      ) g v;
      Hashtbl.replace visited v false
    end
  in
  G.iter_vertex (fun v -> if not (Hashtbl.mem visited v) then visit [] v) g;
;;

let impactset (graph,pkg) = G.fold_pred S.add graph pkg (S.singleton pkg)
let scons (graph,pkg) = G.fold_succ S.add graph pkg (S.singleton pkg)

(* the dominators are computed on the strong dependency graph
 * with transitive archs *)
let dominators_direct ?(relative=None) graph = 
  info "vertex %d - edges %d" (G.nb_vertex graph) (G.nb_edges graph);

  Util.Progress.set_total dombar (G.nb_vertex graph);
  Util.Timer.start domtimer;
  let domgraph = G.create () in
  G.iter_vertex (fun p ->
    Util.Progress.progress dombar;
    let isp = impactset (graph,p) in
    let sconsp = scons (graph,p) in
    G.iter_succ (fun q ->
      if not(CudfAdd.equal p q) then begin
        let isq = impactset (graph,q) in
        let dfs = S.diff isq sconsp in
        match relative with
        |None -> if S.subset dfs isp then G.add_edge domgraph p q
        |Some threshold ->
          let t = ( float ( S.cardinal (S.diff dfs isp)) *. 100.) /. ( float (S.cardinal isp)) in
          if t <= threshold then G.add_edge domgraph p q
      end
    ) graph p
  ) graph;
  debug "cycle reduction"; 
  cycle_reduction domgraph;
  debug "transitive reduction";
  O.transitive_reduction domgraph;
  Util.Timer.stop domtimer domgraph
;;

(* This function expects a strong dependency graph that might or not contain
 * transitive edges *)
let dominators_tarjan graph =
  debug "dominators tarjan";
  debug "vertex %d - edges %d" (G.nb_vertex graph) (G.nb_edges graph);
  let start_pkg = { Cudf.default_package with Cudf.package = "START" } in

  let graph = G.copy graph in

  (* all cycles are cliques in the strong dependency graph *)
  debug "cycle reduction";
  Util.Timer.start crtimer;
  cycle_reduction graph;
  Util.Timer.stop crtimer ();
  debug "vertex %d - edges %d" (G.nb_vertex graph) (G.nb_edges graph);

  debug "transitive reduction";
  Util.Timer.start sdtrtimer;
  O.transitive_reduction graph;
  Util.Timer.stop sdtrtimer ();
  info "vertex %d - edges %d" (G.nb_vertex graph) (G.nb_edges graph);

  (* connect it to all packages without incoming edges to a start vertex *)
  G.iter_vertex (fun v ->
    if (G.in_degree graph v) = 0 then
      G.add_edge graph start_pkg v;
  ) graph;

  debug "tarjan algorithm";
  Util.Timer.start tjntimer;
  let module Dom = Dominator.Make(G) in
  let idom = Dom.compute_all graph start_pkg in
  let domgr = idom.Dom.dom_graph () in
  Util.Timer.stop tjntimer ();

  G.remove_vertex graph start_pkg;
  G.remove_vertex domgr start_pkg;
  
  debug "transitive reduction";
  Util.Timer.start domtrtimer;
  O.transitive_reduction domgr;
  Util.Timer.stop domtrtimer ();
  debug "vertex %d - edges %d" (G.nb_vertex domgr) (G.nb_edges domgr);

  domgr
;;

