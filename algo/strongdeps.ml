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

  open Depsolver_int
  open G

  let strong_depends (universe,maps) p q =
    let size = abs(maps.maps_size - (Cudf.universe_size universe)) in
    let solver = Depsolver_int.init_solver size (universe,maps) in
    let pid = maps.to_sat q in
    let lit = Depsolver_int.S.lit_of_var pid false in
    Depsolver_int.S.add_un_rule solver.constraints lit [];
    match Depsolver_int.edos_install (solver,maps) p with
    |{ Diagnostic.result = Diagnostic.Success _ } -> false
    |_ -> true

  let dependency_graph available =
    let gr = G.create () in
    List.iter (fun (pid,dl,cl) ->
      G.add_vertex gr pid ;
      List.iter (function
        |[p] -> G.add_vertex gr p 
        |l -> List.iter (fun p -> G.add_vertex gr p ) l
      ) dl
    ) available
    ;
    gr

  let conj_dependencies graph maps root =
    let add p1 p2 =
      if p1 <> p2 && not(G.mem_edge graph p1 p2) then begin
        G.add_edge graph p1 p2
      end else begin
        G.iter_succ (fun p ->
          if p <> p1 && not(G.mem_edge graph p1 p) then begin
            G.add_edge graph p1 p ;
          end
        ) graph p2
      end
    in
    let module S = Set.Make(struct type t = Cudf.package let compare = compare end) in
    let queue = Queue.create () in
    let visited = ref S.empty in
    Queue.add (root,[root]) queue;
    while (Queue.length queue > 0) do
      let (pkg,path) = Queue.take queue in
      visited := S.add pkg !visited;
      List.iter (function 
        |[p2] ->
              if not (S.mem p2 !visited) then begin
                Queue.add (p2,pkg::path) queue ;
                add pkg p2 ;
                List.iter(fun p1 -> add p1 p2) (List.rev path)
              end
        |_ -> ()
      ) (List.flatten (
        List.map (List.map maps.lookup_packages) pkg.Cudf.depends))
    done

  let strongdeps available =
    let graph = G.create () in
    let universe = Cudf.load available in
    let maps = Depsolver_int.build_maps universe in
    List.iter (fun pkg1 ->
      G.add_vertex graph pkg1;

      if pkg1.Cudf.depends <> [] then begin
        let pkglist = Depsolver_int.cone maps [pkg1] in
        let cone = Cudf.load pkglist in
        let size = abs(maps.maps_size - (Cudf.universe_size universe)) in
        let solver = Depsolver_int.init_solver size (cone,maps) in
        match Depsolver_int.edos_install (solver,maps) pkg1 with
        |{ Diagnostic.result = Diagnostic.Failure(_) } -> ()
        |{ Diagnostic.result = Diagnostic.Success(f) } -> begin
            conj_dependencies graph maps pkg1;
            List.iter (fun pkg2 ->
              if (pkg1 <> pkg2) && 
                not(G.mem_edge graph pkg1 pkg2) && 
                strong_depends (cone,maps) pkg1 pkg2 then begin
                  G.add_vertex graph pkg2 ;
                  G.add_edge graph pkg1 pkg2
              end
            ) (f ())
        end
      end
    ) available;
    graph

(*

  let strong_pred pr graph p =
    G.iter_pred_e (fun e ->
      if (G.E.label e) = PkgE.Strong then
        let pid = G.E.src e in
        let in_d = G.in_degree graph pid in
        Printf.printf "%s with In degree of %d\n" (pr pid) in_d
    ) graph p

  let sensitivity pr h f graph =
    let ht = Hashtbl.create (G.nb_vertex graph) in
    G.iter_vertex (fun v ->
      G.iter_succ (fun v' ->
        try Hashtbl.replace ht v' ((Hashtbl.find ht v') + 1)
        with Not_found -> Hashtbl.add ht v' 1
      ) graph v
    ) graph;
    Hashtbl.iter (fun p _ ->
      if Hashtbl.mem ht p then
        Printf.printf "%d %s\n" (Hashtbl.find ht p) (pr p)
      else begin
        let dom = dominator pr (dependency_graph (f h p)) p in
        let d =
          List.filter (fun x ->
            try ((Hashtbl.find ht x) >= 5000) && x <> p
            with Not_found -> false
          ) dom
        in
        if d <> [] then begin
          let s = String.concat "," (List.map pr d) in
          Printf.printf "%s is dominated by %s \n" (pr p) s;
          flush_all ()
        end
      end
    ) h
  ;;
*)
end
