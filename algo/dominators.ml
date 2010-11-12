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

let dombar = Util.Progress.create "Algo.dominators"

let debug fmt = Util.make_debug "Dominators" fmt
let info fmt = Util.make_info "Dominators" fmt
let warning fmt = Util.make_warning "Dominators" fmt

module Make (G: Sig.I with type V.t = Cudf.package) = struct

  module C = Components.Make(G)
  module O = Defaultgraphs.GraphOper(G)

  (* to be computed on the strong dependency graph *)
  let _impactset (graph,pkg) =
    G.fold_pred (fun p s ->
      Cudf_set.add p s
    ) graph pkg (Cudf_set.singleton pkg)

  (* to be computed on the strong dependency graph *)
  let _scons (graph,pkg) = 
    G.fold_succ (fun p s ->
      Cudf_set.add p s
    ) graph pkg (Cudf_set.singleton pkg)

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
  begin
    info "N. of vertex graph %d\n" (G.nb_vertex graph);
    info "N. of edges graph %d\n" (G.nb_edges graph);
    
    let domtimer = Util.Timer.create ~enabled:true "Algo.Dominators.dominators" in

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
          let dfs = Cudf_set.diff isq sconsp in
          match relative with
          | None -> 
            if Cudf_set.subset dfs isp then begin
              G.add_edge domgraph p q;
              debug "Dominator %s -D-> %s !"
              (CudfAdd.print_package p)
              (CudfAdd.print_package q);
            end
          | Some f -> 
            let fv = (float (Cudf_set.cardinal (Cudf_set.diff dfs isp)) *. 100.) /. (float (Cudf_set.cardinal isp)) in
            if fv <= f then begin
              G.add_edge domgraph p q;
              debug "Dominator %s -D-> %s !"
              (CudfAdd.print_package p)
              (CudfAdd.print_package q);
            end
        end
      ) graph p
    ) graph;
    Util.Timer.stop domtimer domgraph
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
            package = String.concat "/" (List.sort (List.map (fun p -> p.package) n));
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

    (* inspired by has_cycle from ocamlgraph; not in hashtbl: not visited,
     * false in hashtbl: visited in another component, true in hashtbl:
     * visited here *)
    let cycle_reduction g =
      let visited = Hashtbl.create (G.nb_vertex g) in
      let rec get_cycle res path v' =
        match path with
        | [] -> failwith "No cycle in path!"
        | h::t -> if h = v' then (t, res) else get_cycle (h::res) t v' in
      let reduce_cycle path v' =
      begin
        (* Somewhere, v' should be in path. This is the cycle. *)
        let (other, c) = get_cycle [] path v' in
        (* Printf.eprintf "Replacing cycle: %s %s\n" v'.package
          (String.concat "," (List.map (fun p -> p.package) c)); *)
        let nv = {
          package = String.concat "/" (List.sort (List.map (fun p -> p.package) (v'::c)));
          version = 0;
          depends = [];
          conflicts = [];
          provides = [];
          installed = false;
          was_installed = false;
          keep = `Keep_none;
          pkg_extra = []
        } in
        G.add_vertex g nv;
        List.iter (fun p ->
          G.iter_pred (fun p' ->
            if not (List.mem p' c) then
              G.add_edge g p' nv
          ) g p;
          G.iter_succ (fun p' ->
            if not (List.mem p' c) then
              G.add_edge g nv p'
          ) g p;
          G.remove_vertex g p;
          Hashtbl.remove visited p
        ) (v'::c);
        (other, nv)
      end in
      let rec visit path v =
      begin
        if G.mem_vertex g v then
        begin
          (* Printf.eprintf "- visiting [%s] %s\n" (String.concat "," (List.map (fun p -> p.package) path)) v.package; *)
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
          (* Printf.eprintf "- done visiting %s\n" v.package; *)
          Hashtbl.replace visited v false
        end
        (* else
          Printf.eprintf "- visiting %s, but it's already been removed!\n" v.package *)
      end in
    begin
      G.iter_vertex (fun v ->
        if not (Hashtbl.mem visited v) then visit [] v
      ) g 
    end


    module T = Traverse.Dfs(G)

    let dominators_tarjan g =
    begin
      let graph = G.copy g in
      Printf.eprintf "sd_graph before reduction: %d vertices, %d edges\n" (G.nb_vertex graph) (G.nb_edges graph);
      cycle_reduction graph;
      O.transitive_reduction graph;
      Printf.eprintf "sd_graph after reduction: %d vertices, %d edges\n" (G.nb_vertex graph) (G.nb_edges graph);
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
        let tjntimer = Util.Timer.create ~enabled:true "Algo.Dominators.tarjan" in
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
          debug "step 2 for vertex %s...%!" w.package;
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
                debug "step 3 for vertex %s...%!" w.package;
                let u = eval v in
                (match (try G.pred domgr v with Invalid_argument _ -> []) with
                | [] -> ()
                | [p] -> G.remove_edge domgr p v
                | _ -> failwith (Printf.sprintf "Vertex %s has multiple dominators" v.package));
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
          | _ -> failwith (Printf.sprintf "Vertex %s has multiple predecessors in spanning tree" w.package)
        ) !vertex_order;
        (* step 4 *)
        List.iter (fun w ->
          debug "step 4 for %s...%!" w.package;
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
        Util.Timer.stop tjntimer domgr
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
