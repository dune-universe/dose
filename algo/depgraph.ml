(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

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
