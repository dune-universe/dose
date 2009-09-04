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

module Make (G: Sig.I with type V.t = Cudf.package) = struct
  module S = Set.Make(struct type t = G.V.t let compare = compare end)

  (* to be computed on the strong dependency graph *)
  let _impactset graph p =
    G.fold_pred (fun v s ->
      S.add v s
    ) graph p (S.singleton p)

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
  let _scons graph p = 
    List.fold_left (fun s e -> 
      S.add e s
    ) S.empty (G.succ graph p)

  let impactset = memo _impactset 
  let scons = memo _scons 

  let stats max = 
    let curr = ref 0 in
    let step = 10 in
    fun i ->
      incr curr;
      if !curr >= step then begin
        Printf.printf "Done %d out of %d\n" i max;
        flush_all ();
        curr := 0
      end
  ;;

  (* z -> p -> q 
   * if z -> q && z -> p && q \in Succ(p) then p -> q 
   *)
  let transcond domgraph p q = 
    List.exists (fun z ->
      (G.mem_edge domgraph z p) && (G.mem_edge domgraph z q)
    ) (G.pred domgraph q)

  (* the dominators are computed on the strong dependency graph
   * with transitive archs *)
  let dominators pr graph =
    Printf.printf "N. of vertex graph %d\n" (G.nb_vertex graph);
    Printf.printf "N. of edges graph %d\n" (G.nb_edges graph);
    print_endline "start dominators";
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
        if not(transcond domgraph p q) then begin
          let isq = impactset graph q in
          if S.subset (S.diff isq sconsp) isp then begin
            G.add_edge domgraph p q;
            Printf.printf "Dominator %s -D-> %s !\n" (pr p) (pr q)
          end
        end else begin
            G.add_edge domgraph p q;
            Printf.printf "Dominator %s -D-> %s !\n" (pr p) (pr q)
        end
      ) graph p
    ) graph


(*

let dominator pr gr root =
  let module T = Graph.Traverse.Dfs(G) in
  let dom = Hashtbl.create (G.nb_vertex gr) in
  G.iter_vertex (fun v -> Hashtbl.add dom v S.empty) gr ;
  let change = ref true in
  while !change do
    change := false ;
    T.postfix_component (fun n ->
      let newset =
        let pred = (G.pred gr n) in
        let rec inter = function
          |[h] -> S.singleton h
          |h::t -> S.inter (Hashtbl.find dom h) (inter t)
          |[] -> S.empty
        in
        S.union (inter pred) (S.singleton n)
      in
      if not (S.equal newset (Hashtbl.find dom n)) then begin
        Hashtbl.replace dom n newset ;
        change := true
      end
    ) gr root
  done;
  (S.elements (Hashtbl.find dom root))
;;

let print_dom pr graph root =
  G.iter_vertex (fun pid ->
    let dom = dominator pr graph pid in
    let s = String.concat "," (List.map pr dom) in
    Printf.printf "%s dominance degree of %d : %s\n" (pr pid) ((List.length dom) -1) s
  ) graph
;;

*)
end
