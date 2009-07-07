
open Graph
open ExtLib

module Make (G: Sig.I) = struct
  module S = Set.Make(struct type t = G.V.t let compare = compare end)

  (* to be computed on the strong dependency graph *)
  let _impactset graph p =
    G.fold_pred (fun v s ->
      S.add v s
    ) graph p (S.singleton p)
  ;;

  let memo f = 
    let h = Hashtbl.create 1031 in
    fun graph -> fun p ->
      try Hashtbl.find h p
      with Not_found -> begin
        let r = f graph p in
        Hashtbl.add h p r;
        r
      end
  ;;

  (* to be computed on the strong dependency graph *)
  let _scons graph p = 
    List.fold_left (fun s e -> 
      S.add e s
    ) S.empty (G.succ graph p)
  ;;

  let impactset = memo _impactset ;;
  let scons = memo _scons ;;

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
  ;;
end
