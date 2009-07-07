
  let dependency_graph available =
    let gr = G.create () in
    List.iter (fun (pid,dl,cl) ->
      G.add_vertex gr pid ;
      List.iter (function
        |[p] ->
            begin
              G.add_vertex gr p ;
              G.add_edge_e gr (G.E.create pid PkgE.Direct p)
            end
        |l ->
            List.iter (fun p ->
              G.add_vertex gr p ;
              G.add_edge_e gr (G.E.create pid PkgE.Disjunctive p)
            ) l
      ) dl
    ) available
    ;
    gr
  ;;


  (* ----------------------------------- *)

module Make(G : Graph.Sig.G) = struct

  (* side effect, this function modifies the input graph *)
  let transitive_reduction graph =
    G.iter_vertex (fun v ->
      List.iter (fun v' ->
        if v <> v' then
          List.iter (fun v'' ->
            if v' <> v'' then
              G.remove_edge graph v v''
          ) (G.succ graph v')
      ) (G.succ graph v);
    ) graph ;
    graph
  ;;

end
