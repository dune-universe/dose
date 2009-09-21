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

let mainbar = Util.Progress.create "Algo.Strongdep.main"
let conjbar = Util.Progress.create "Algo.Strongdep.conj"

module Make (G: Sig.I with type V.t = int) = struct

  open Depsolver_int
  open G

  (* does p strongly dependens on q ?
   * we check if it is possible to install p without q.
   
   * dependes on q *)
  (* ATT: this function makes a copy of the solver to 
   * add a clause to it *)
  let strong_depends solver p q =
    Depsolver_int.S.reset solver.constraints; 
    let solver = Depsolver_int.copy_solver solver in 
    let lit = Depsolver_int.S.lit_of_var (solver.map#vartoint q) false in
    Depsolver_int.S.add_un_rule solver.constraints lit [];
    match Depsolver_int.solve solver (Diagnostic_int.Sng p) with
    |Diagnostic_int.Failure _ -> true
    |Diagnostic_int.Success _ -> false

  let conj_dependencies graph index l =
    let rec __conj_dependencies path =
      let id = List.hd path in
      let altlist = index.(id).Mdf.depends in
      Array.iter (function
        |(_,pa,_) when Array.length pa = 1 ->
            let p = pa.(0) in
            if G.mem_edge graph id p then begin
              List.iter (fun id1 ->
                G.iter_succ (fun id2 ->
                  if not(id1 = id2) then 
                    G.add_edge graph id1 id2
                ) graph p
              ) path 
            end else begin
              List.iter (fun id1 ->
                if not(id1 = p) then
                  G.add_edge graph id1 p
              ) path;
              if not(List.mem p path) then
                __conj_dependencies (p::path)
            end
        |_ -> ()
      ) altlist
    in try __conj_dependencies l with _ -> assert false

  let strongdeps pkglist =
    let graph = G.create () in
    let mdf = Mdf.load_from_list pkglist in
    let size = List.length pkglist in
    Util.Progress.set_total mainbar size;
    Util.Progress.set_total conjbar size;
    let timer = Util.Timer.create "Algo.Strongdep" in
    Util.Timer.start timer;
    let available = 
      let dummy = (Mdf.default_package,0,[]) in
      let a = Array.create size dummy in
      for id = 0 to size - 1 do
        let pkg = mdf.Mdf.index.(id) in
        Util.Progress.progress conjbar;
        conj_dependencies graph mdf.Mdf.index [id]; 
        let closure = dependency_closure mdf.Mdf.index [id] in 
        a.(id) <- (pkg,List.length closure,closure)
      done ;
      a
    in
    Array.sort (fun (_,n,_) (_,m,_) -> m - n) available;  

    for i = 0 to (Array.length available) -1 do
      let (pkg1,_,closure) = available.(i) in
      let pkg1_id = pkg1.Mdf.id in
      Util.Progress.progress mainbar;
      if Array.length pkg1.Mdf.depends > 0 then begin
        let solver = Depsolver_int.init_solver ~idlist:closure mdf.Mdf.index in
        match Depsolver_int.solve solver (Diagnostic_int.Sng pkg1_id) with
        |Diagnostic_int.Failure(_) -> ()
        |Diagnostic_int.Success(f) -> begin
          (* Printf.eprintf "Package: %s\n%!" (CudfAdd.print_package pkg1.Mdf.pkg);
          Printf.eprintf "Closure size: %d\n%!" (List.length closure);
          Printf.eprintf "IS size: %d\n%!" (List.length (f()));
          *)
          let i = ref 0 in
          List.iter (fun pkg2_id ->
            if not(pkg1_id = pkg2_id) then
              if not(G.mem_edge graph pkg1_id pkg2_id) then
                if strong_depends solver pkg1_id pkg2_id then 
                  (incr i ; G.add_edge graph pkg1_id pkg2_id)
          ) (f ())
(*
          let inter a b = List.filter (fun i -> List.mem i b) a in
          let is = f () in
          List.iter (fun p ->
            let closure = let (_,_,c) = available.(p) in c in
            List.iter (fun q ->
              if not(p = q) && not(G.mem_edge graph p q) then
                if strong_depends solver p q then 
                  (incr i ; G.add_edge graph p q)
            ) (inter closure is)
          ) is
*)
(* ; Printf.eprintf "n strong dep: %d\n%!" !i; *)
        end
      end
    done ;
    Util.Timer.stop timer graph

(* 
  let strong_pred graph q =
    G.iter_pred_e (fun edge ->
      let p = G.E.src edge in
      let in_d = G.in_degree graph p in
      Printf.printf "%s with In degree of %d\n" (CudfAdd.print_package p) in_d
    ) graph q

  let dependency_graph available =
  let gr = G.make (List.length available) in
  List.iter (fun (pid,dl,cl) ->
    G.add_vertex gr pid ;
    List.iter (function
      |[p] -> G.add_vertex gr p 
      |l -> List.iter (fun p -> G.add_vertex gr p ) l
    ) dl
  ) available
  ;
  gr

  let sensitivity h f graph =
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
*)
end
