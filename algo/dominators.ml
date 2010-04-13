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
open CudfAdd
open Cudf

module Make (G: Sig.I with type V.t = Cudf.package) = struct

  module C = Components.Make(G)
  module O = Defaultgraphs.GraphOper(G)

  (* to be computed on the strong dependency graph *)
  let _impactset graph pkg =
    G.fold_pred (fun p s ->
      Cudf_set.add p s
    ) graph pkg (Cudf_set.singleton pkg)

  let memo f = 
    let h = Hashtbl.create 1031 in
    fun graph -> fun p ->
      try Hashtbl.find h p
      with Not_found -> begin
        let r = f graph p in
        Hashtbl.add h p r;
        r
      end

  (* to be computed on the strong dependency graph *)
  let _scons graph pkg = 
    List.fold_left (fun s p -> 
      Cudf_set.add p s
    ) Cudf_set.empty (G.succ graph pkg)

  let impactset = memo _impactset 
  let scons = memo _scons 

  let stats max = 
    let curr = ref 0 in
    let step = 10 in
    fun i ->
      incr curr;
      if !curr >= step then begin
        Util.print_info "Done %d out of %d%!" i max;
        curr := 0
      end
  ;;

  (* the dominators are computed on the strong dependency graph
   * with transitive archs *)
  let dominators graph = 
  begin
    Util.print_info "N. of vertex graph %d" (G.nb_vertex graph);
    Util.print_info "N. of edges graph %d" (G.nb_edges graph);
    Util.print_info "start dominators";
    let i = ref 0 in
    let stats = stats (G.nb_vertex graph) in
    let domgraph = G.create () in
    G.iter_vertex (fun p ->
      G.add_vertex domgraph p;
      let isp = impactset graph p in
      let sconsp = scons graph p in
      stats !i; incr i;
      G.iter_succ (fun q ->
        if p <> q then
        begin
          G.add_vertex domgraph q;
          let isq = impactset graph q in
          let dfs = Cudf_set.diff isq sconsp in
          if Cudf_set.subset dfs isp then begin
            G.add_edge domgraph p q;
            Util.print_info "Dominator %s -D-> %s !"
            (CudfAdd.print_package p)
            (CudfAdd.print_package q);
          end
        end
      ) graph p
    ) graph;
    domgraph
  end

    (** clique reduction: replace any clique by a fresh node.
        If we do this before transitive reduction, any cycle will be a clique
        and thus this will also perform cycle reduction. *)
    let clique_reduction graph =
    begin
      List.iter (function
      | [] -> ()
      | [_] -> ()
      | n ->
        begin 
          let nv = {
            package = String.concat "," (List.sort (List.map (fun p -> p.package) n));
            version = 0;
            depends = [];
            conflicts = [];
            provides = [];
            installed = false;
            was_installed = false;
            keep = `Keep_none;
            pkg_extra = []
          } in
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
    end

    let dominators_tarjan g =
    begin
      let graph = G.copy g in
      clique_reduction graph;
      O.transitive_reduction graph;
      let start_pkg = 
        { package = "START"; version = 0; depends = []; conflicts = [];
          provides = []; installed = false; was_installed = false;
          keep = `Keep_none; pkg_extra = [] } in
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
        ) graph v
      end in

      let link v w =
        G.add_edge forest v w in

      let rec compress_path res v =
      begin
        match G.pred forest v with
        | [] -> res
        | [p] ->
          if smaller_number (Hashtbl.find semi_ht res) (Hashtbl.find semi_ht p)
          then
          begin
            match G.pred forest p with
            | [] -> res
            | [_] -> compress_path res p
            | _ -> failwith (Printf.sprintf "Vertex %s has multiple predecessors in forest" p.package)
          end
          else
            compress_path p p
        | _ -> failwith (Printf.sprintf "Vertex %s has multiple predecessors in forest" v.package)
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
          Common.Util.print_info "step 2 for vertex %s...%!" w.package;
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
                Common.Util.print_info "step 3 for vertex %s...%!" w.package;
                let u = eval v in
                (match (try G.pred domgr v with Invalid_argument _ -> []) with
                | [] -> ()
                | [p] -> G.remove_edge domgr p v
                | _ -> failwith (Printf.sprintf "Vertex %s has multiple dominators" v.package));
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
          | _ -> failwith (Printf.sprintf "Vertex %s has multiple predecessors in spanning tree" w.package)
        ) !vertex_order;
        (* step 4 *)
        List.iter (fun w ->
          Common.Util.print_info "step 4 for %s...%!" w.package;
          match (try G.pred domgr w with Invalid_argument _ -> []) with
          | [] -> ()
          | [p] -> if compare p (Hashtbl.find semi_ht w) <> 0 then
                  begin
                    match (try G.pred domgr p with Invalid_argument _ -> []) with
                    | [] -> ()
                    | [p_p] -> (G.remove_edge domgr p w; G.add_edge domgr p_p w)
                    | _ -> failwith (Printf.sprintf "Vertex %s has multiple dominators" p.package)
                  end
          | _ -> failwith (Printf.sprintf "Vertex %s has multiple dominators" w.package)
        ) (List.rev !vertex_order);
        domgr
      end
  end

(*
  let print_dom graph root =
    G.iter_vertex (fun pkg ->
      let dom = dominators graph pkg in
      let s = String.concat "," (List.map Diagnostic.print_package dom) in
      Printf.printf "%s dominance degree of %d : %s\n"
      (Diagnostic.print_package pkg)
      ((List.length dom) -1) s
    ) graph
*)
end
