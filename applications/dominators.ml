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

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()

  let description = "Compute the dominator graph"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
end

module G = StrongDepGraph.G;;
module O = StrongDepGraph.O;;
module C = Components.Make(G);;

(* ----------------------------------- *)

(** clique reduction: replace any clique by a fresh node.
    If we do this before transitive reduction, any cycle will be a clique
    and thus this will also perform cycle reduction. *)
let clique_reduction (graph: StrongDepGraph.G.t) =
begin
  List.iter (function
  | [] -> ()
  | [_] -> ()
  | n ->
    begin 
      let nv = (String.concat "," (List.map fst n), "0") in
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
end;;

let tarjan graph =

let start_pkg = ("START", "0") in
let vertex_order = ref [] in
let n = ref 1 in
let vertex_number_ht = Hashtbl.create (G.nb_vertex graph) in
let semi_ht = Hashtbl.create (G.nb_vertex graph) in
let stg = G.create () in
let forest = G.create () in
let domgr = G.create () in

let smaller_number x y =
  Hashtbl.find vertex_number_ht x < Hashtbl.find vertex_number_ht y in

let rec dfs v =
begin
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
  ) graph v;
end in

let link v w =
  G.add_edge forest v w in

let rec compress_path res v =
begin
  match G.pred forest v with
  | [] -> res
  | [p] ->
    if smaller_number (Hashtbl.find semi_ht res) (Hashtbl.find semi_ht p) then
    begin
      match G.pred forest p with
      | [] -> res
      | [_] -> compress_path res p
      | _ -> failwith (Printf.sprintf "Vertex %s has multiple predecessors in forest" (fst p))
    end
    else
      compress_path p p
  | _ -> failwith (Printf.sprintf "Vertex %s has multiple predecessors in forest" (fst v))
end in

let eval v =
begin
  G.add_vertex forest v;
  if G.in_degree forest v = 0 then
    v
  else
    compress_path v v
end in

begin
  (* add a start vertex, and connect it to all packages without incoming edges
   *)
  G.add_vertex graph start_pkg;
  G.iter_vertex (fun v ->
    if compare v start_pkg <> 0 then
    begin
      if (try G.in_degree graph v with Invalid_argument _ -> 0) = 0 then
        G.add_edge graph start_pkg v;
    end
  ) graph;
  Printf.printf "starting Tarjan algorithm...\n%!";
  dfs start_pkg;
  let bucket_ht = Hashtbl.create (G.nb_vertex graph) in
  (* step 2 and 3 *)
  List.iter (fun w ->
    Common.Util.print_info "step 2 for vertex %s...\n%!" (fst w);
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
          Common.Util.print_info "step 3 for vertex %s...\n%!" (fst w);
          let u = eval v in
          (match (try G.pred domgr v with Invalid_argument _ -> []) with
          | [] -> ()
          | [p] -> G.remove_edge domgr p v
          | _ -> failwith (Printf.sprintf "Vertex %s has multiple dominators" (fst v)));
          if smaller_number (Hashtbl.find semi_ht u) (Hashtbl.find semi_ht u) then
            G.add_edge domgr u v
          else
            G.add_edge domgr parent_w v
        ) (Hashtbl.find_all bucket_ht parent_w);
        while Hashtbl.mem bucket_ht parent_w
        do
          Hashtbl.remove bucket_ht parent_w
        done;
      end
    | _ -> failwith (Printf.sprintf "Vertex %s has multiple predecessors in spanning tree" (fst w))
  ) !vertex_order;
  (* step 4 *)
  List.iter (fun w ->
    Common.Util.print_info "step 4 for %s...\n%!" (fst w);
    match (try G.pred domgr w with Invalid_argument _ -> []) with
    | [] -> ()
    | [p] -> if compare p (Hashtbl.find semi_ht w) <> 0 then
             begin
               match (try G.pred domgr p with Invalid_argument _ -> []) with
               | [] -> ()
               | [p_p] -> (G.remove_edge domgr p w; G.add_edge domgr p_p w)
               | _ -> failwith (Printf.sprintf "Vertex %s has multiple dominators" (fst p))
             end
    | _ -> failwith (Printf.sprintf "Vertex %s has multiple dominators" (fst w))
  ) (List.rev !vertex_order);
  StrongDepGraph.D.output_graph stdout domgr
end;;

let () =
begin
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug ();
  let (universe,_,_) = Boilerplate.load_universe posargs in

  let sd_graph = StrongDepGraph.transform_out (Strongdeps.strongdeps_univ universe) in
  Printf.eprintf "Before transformation: %d nodes, %d edges\n" (G.nb_vertex sd_graph) (G.nb_edges sd_graph);
  clique_reduction sd_graph;
  O.transitive_reduction sd_graph;
  Printf.eprintf "After transformation: %d nodes, %d edges\n" (G.nb_vertex sd_graph) (G.nb_edges sd_graph);
  tarjan sd_graph
end;;
