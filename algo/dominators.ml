(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

open Graph
open ExtLib
open Common
open CudfAdd

module Make (G: Sig.I with type V.t = Cudf.package) = struct

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
        Util.print_info "Done %d out of %d\n%!" i max;
        curr := 0
      end
  ;;

  (* the dominators are computed on the strong dependency graph
   * with transitive archs *)
  let dominators graph =
    Util.print_info "N. of vertex graph %d\n" (G.nb_vertex graph);
    Util.print_info "N. of edges graph %d\n" (G.nb_edges graph);
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
        G.add_vertex domgraph q;
        let isq = impactset graph q in
        if Cudf_set.subset (Cudf_set.diff isq sconsp) isp then begin
          G.add_edge domgraph p q;
          Util.print_info "Dominator %s -D-> %s !\n"
          (CudfAdd.print_package p)
          (CudfAdd.print_package q)
        end
      ) graph p
    ) graph

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
