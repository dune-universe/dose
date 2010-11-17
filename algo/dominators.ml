(**************************************************************************************)
(*  Copyright (C) 2009-2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                      and Jaap Boender <boender@pps.jussieu.fr>                          *)
(*  Copyright (C) 2009-2010 Mancoosi Project                                               *)
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

let debug fmt = Util.make_debug "Dominators" fmt
let info fmt = Util.make_info "Dominators" fmt
let warning fmt = Util.make_warning "Dominators" fmt
let fatal fmt = Util.make_fatal "Dominators" fmt

module G = Defaultgraphs.PackageGraph.G
module C = Components.Make(G)
module O = Defaultgraphs.GraphOper(G)
module S = CudfAdd.Cudf_set

(* to be computed on the strong dependency graph *)
let _impactset (graph,pkg) =
  G.fold_pred (fun p s ->
    S.add p s
  ) graph pkg (S.singleton pkg)

(* to be computed on the strong dependency graph *)
let _scons (graph,pkg) = 
  G.fold_succ (fun p s ->
    S.add p s
  ) graph pkg (S.singleton pkg)

let impactset graph pkg = Util.memo _impactset (graph,pkg)
let scons graph pkg = Util.memo _scons (graph,pkg)

let stats max = 
  let curr = ref 0 in
  let step = 10 in
  fun i ->
    incr curr;
    if !curr >= step then begin
      debug "Done %d out of %d" i max;
      curr := 0
    end
;;

(* the dominators are computed on the strong dependency graph
 * with transitive archs *)
let dominators ?relative graph = 
  info "N. of vertex graph %d\n" (G.nb_vertex graph);
  info "N. of edges graph %d\n" (G.nb_edges graph);
  
  let domtimer = Util.Timer.create "Algo.Dominators.dominators" in

  Util.Progress.set_total dombar (G.nb_vertex graph);
  Util.Timer.start domtimer;
  let i = ref 0 in
  let stats = stats (G.nb_vertex graph) in
  let domgraph = G.create () in
  G.iter_vertex (fun p ->
    G.add_vertex domgraph p;
    Util.Progress.progress dombar;
    let isp = impactset graph p in
    let sconsp = scons graph p in
    stats !i; incr i;
    G.iter_succ (fun q ->
      if p <> q then
      begin
        G.add_vertex domgraph q;
        let isq = impactset graph q in
        let dfs = S.diff isq sconsp in
        match relative with
        | None -> 
          if S.subset dfs isp then begin
            G.add_edge domgraph p q;
            debug "Dominator %s -D-> %s !" (CudfAdd.print_package p) (CudfAdd.print_package q);
          end
        | Some f -> 
          let fv = ( float ( S.cardinal (S.diff dfs isp)) *. 100.) /. ( float (S.cardinal isp)) in
          if fv <= f then begin
            G.add_edge domgraph p q;
            debug "Dominator %s -D-> %s !" (CudfAdd.print_package p) (CudfAdd.print_package q);
          end
      end
    ) graph p
  ) graph;
  Util.Timer.stop domtimer domgraph
;;

(** clique reduction: replace any clique by a fresh node.
    If we do this before transitive reduction, any cycle will be a clique
    and thus this will also perform cycle reduction. *)
let clique_reduction graph =
  List.iter (function
  | [] -> ()
  | [_] -> ()
  | n ->
    begin 
      let nv = {
        Cudf.default_package with 
        Cudf.package = String.concat "/" (List.sort (List.map (fun p -> p.Cudf.package) n)) } 
      in
      G.add_vertex graph nv;
      List.iter (fun p ->
        G.iter_pred (fun p' ->
          if not (List.mem p' n) then
            G.add_edge graph p' nv
        ) graph p;
        G.iter_succ (fun p' ->
          if not (List.mem p' n) then
            G.add_edge graph nv p'
        ) graph p;
        G.remove_vertex graph p
      ) n;
    end
  ) (C.scc_list graph)
;;

(* inspired by has_cycle from ocamlgraph; not in hashtbl: not visited,
 * false in hashtbl: visited in another component, true in hashtbl:
 * visited here *)
let cycle_reduction g =
  let visited = Hashtbl.create (G.nb_vertex g) in
  let rec get_cycle res path v' =
    match path with
    | [] -> fatal "No cycle in path!"
    | h::t -> if h = v' then (t, res) else get_cycle (h::res) t v' 
  in
  let reduce_cycle path v' =
    (* Somewhere, v' should be in path. This is the cycle. *)
    let (other, c) = get_cycle [] path v' in
    let nv = { 
      Cudf.default_package with 
      Cudf.package = String.concat "/" (List.sort (List.map (fun p -> p.Cudf.package) (v'::c))) } 
    in
    G.add_vertex g nv;
    List.iter (fun p ->
      G.iter_pred (fun p' -> if not (List.mem p' c) then G.add_edge g p' nv) g p;
      G.iter_succ (fun p' -> if not (List.mem p' c) then G.add_edge g nv p') g p;
      G.remove_vertex g p;
      Hashtbl.remove visited p
    ) (v'::c);
    (other, nv)
  in
  let rec visit path v =
    if G.mem_vertex g v then
    begin
      Hashtbl.add visited v true;
      G.iter_succ (fun v' ->
        try
          if Hashtbl.find visited v' then
          begin
            let (other, nv) = reduce_cycle (v::path) v' in
            visit other nv
          end
        with Not_found ->
          visit (v::path) v'
      ) g v;
      Hashtbl.replace visited v false
    end
  in
  G.iter_vertex (fun v -> if not (Hashtbl.mem visited v) then visit [] v) g 
;;


module T = Traverse.Dfs(G)

let dominators_tarjan g =
  let graph = G.copy g in
  debug "sd_graph before reduction: %d vertices, %d edges\n" (G.nb_vertex graph) (G.nb_edges graph);
  cycle_reduction graph;
  O.transitive_reduction graph;
  debug "sd_graph after reduction: %d vertices, %d edges\n" (G.nb_vertex graph) (G.nb_edges graph);
  let start_pkg = { Cudf.default_package with Cudf.package = "START" } in
  let vertex_order = ref [] in
  let n = ref 1 in
  let vertex_number_ht = Hashtbl.create (G.nb_vertex graph) in
  let semi_ht = Hashtbl.create (G.nb_vertex graph) in
  let stg = G.create () in
  let forest = G.create () in
  let domgr = G.create () in

  let smaller_number x y =
    Hashtbl.find vertex_number_ht x < Hashtbl.find vertex_number_ht y 
  in

  let rec dfs v =
    vertex_order := v::!vertex_order;
    Hashtbl.replace semi_ht v v; (* v is its own semi-dominator for now *)
    Hashtbl.replace vertex_number_ht v !n;
    incr n;
    G.iter_succ (fun w ->
      if not (Hashtbl.mem semi_ht w) then
      begin
        (* v does not yet have a semi-dominator *)
        G.add_edge stg v w;
        dfs w
      end
    ) graph v
  in

  let link v w = G.add_edge forest v w in

  let rec compress_path res v =
    match G.pred forest v with
    | [] -> res
    | [p] ->
      if smaller_number (Hashtbl.find semi_ht res) (Hashtbl.find semi_ht p)
      then
      begin
        match G.pred forest p with
        | [] -> res
        | [_] -> compress_path res p
        | _ -> fatal "Vertex %s has multiple predecessors in forest" (CudfAdd.print_package p)
      end
      else
        compress_path p p
    | _ -> fatal "Vertex %s has multiple predecessors in forest" (CudfAdd.print_package v)
  in
  
  let eval v =
    G.add_vertex forest v;
    if G.in_degree forest v = 0 then v 
    else compress_path v v
  in
  
  let tjntimer = Util.Timer.create "Algo.Dominators.tarjan" in
  Util.Timer.start tjntimer;

  (* add a start vertex, and connect it to all packages without
   * incoming edges *)
  G.add_vertex graph start_pkg;
  G.iter_vertex (fun v ->
    if compare v start_pkg <> 0 then
    begin
      if (try G.in_degree graph v with Invalid_argument _ -> 0) = 0 then
        G.add_edge graph start_pkg v;
    end
  ) graph;
  dfs start_pkg;
  let bucket_ht = Hashtbl.create (G.nb_vertex graph) in
  (* step 2 and 3 *)
  List.iter (fun w ->
    debug "step 2 for vertex %s...%!" (CudfAdd.print_package w);
    G.iter_pred (fun v ->
      let u = eval v in
      let semi_u = Hashtbl.find semi_ht u in
      if smaller_number semi_u (Hashtbl.find semi_ht w) then
        Hashtbl.replace semi_ht w semi_u;      
    ) graph w;
    Hashtbl.add bucket_ht (Hashtbl.find semi_ht w) w;
    match (try G.pred stg w with Invalid_argument _ -> []) with
    | [] -> ()
    | [parent_w] ->
      begin
        link parent_w w;
        List.iter (fun v ->
          debug "step 3 for vertex %s...%!" (CudfAdd.print_package w);
          let u = eval v in
          (match (try G.pred domgr v with Invalid_argument _ -> []) with
          | [] -> ()
          | [p] -> G.remove_edge domgr p v
          | _ -> fatal "Vertex %s has multiple dominators" (CudfAdd.print_package v));
          if smaller_number (Hashtbl.find semi_ht u) (Hashtbl.find semi_ht v) then
            G.add_edge domgr u v
          else
            G.add_edge domgr parent_w v
        ) (Hashtbl.find_all bucket_ht parent_w);
        while Hashtbl.mem bucket_ht parent_w
        do
          Hashtbl.remove bucket_ht parent_w
        done;
      end
    | _ -> fatal "Vertex %s has multiple predecessors in spanning tree" (CudfAdd.print_package w)
  ) !vertex_order;
  (* step 4 *)
  List.iter (fun w ->
    debug "step 4 for %s...%!" (CudfAdd.print_package w);
    match (try G.pred domgr w with Invalid_argument _ -> []) with
    | [] -> ()
    | [p] when (compare p (Hashtbl.find semi_ht w) <> 0) ->
        begin
          match (try G.pred domgr p with Invalid_argument _ -> []) with
          | [] -> ()
          | [p_p] -> (G.remove_edge domgr p w; G.add_edge domgr p_p w)
          | _ -> fatal "Vertex %s has multiple dominators" (CudfAdd.print_package p)
        end
    | _ -> fatal "Vertex %s has multiple dominators" (CudfAdd.print_package w)
  ) (List.rev !vertex_order);
  Util.Timer.stop tjntimer domgr
;;

