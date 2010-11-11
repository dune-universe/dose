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

(** Strong Conflicts *)

open Graph
open ExtLib
open Common
open CudfAdd

let debug fmt = Util.make_debug "Strongconflicts_int" fmt
let info fmt = Util.make_info "Strongconflicts_int" fmt
let warning fmt = Util.make_warning "Strongconflicts_int" fmt

module SG = Strongdeps_int.G
module PkgV = struct
  type t = int
  let compare = Pervasives.compare
  let hash i = i
  let equal = (=)
end
type cfl_type = Explicit | Conjunctive | Other of Diagnostic_int.reason list;;
module CflE = struct
  type t = int * int * cfl_type
  let compare = Pervasives.compare
  let default = (0, 0, Other []) 
end

(* unlabelled indirected graph, for the cache *)
module IG = Graph.Imperative.Graph.Concrete(PkgV)
module CG = Graph.Imperative.Graph.ConcreteLabeled(PkgV)(CflE)

(** progress bar *)
let seedingbar = Util.Progress.create "Algo.Strongconflicts.seeding" ;;
let localbar = Util.Progress.create "Algo.Strongconflicts.local" ;;

open Depsolver_int

module S = Set.Make (struct type t = int let compare = Pervasives.compare end)
type cl = { rdc : S.t ; rd : S.t } 

let swap (p,q) = if p < q then (p,q) else (q,p) ;;
let to_set l = List.fold_right S.add l S.empty ;;

let explicit mdf =
  (* let cmp (x : int * int) (y : int * int) = x = y in *)
  let index = mdf.Mdf.index in
  let l = ref [] in
  for i=0 to (Array.length index - 1) do
    let pkg = index.(i) in
    let conflicts = List.map snd pkg.Mdf.conflicts in
    List.iter (fun j ->
      l := swap(i,j):: !l
    ) conflicts
  done;
  (* List.unique ~cmp !l *)
  Util.list_unique !l

;;

(* [strongconflicts mdf] return the list of strong conflicts
   @param mdf
*)
let strongconflicts mdf =
  let index = mdf.Mdf.index in
  let solver = Depsolver_int.init_solver index in
  let reverse = reverse_dependencies mdf in
  let size = (Array.length index) in
  let cache = IG.create ~size:size () in
  let cl_dummy = {rdc = S.empty ; rd = S.empty} in
  let closures = Array.create size cl_dummy in

  debug "Pre-seeding ...";

  Util.Progress.set_total seedingbar (Array.length mdf.Mdf.index);

  let cg = SG.create ~size () in
  for i=0 to (size - 1) do
    Util.Progress.progress seedingbar;
    let rdc = to_set (reverse_dependency_closure reverse [i]) in
    let rd = to_set reverse.(i) in
    closures.(i) <- {rdc = rdc; rd = rd};
    Defaultgraphs.IntPkgGraph.conjdepgraph_int cg index i ; 
    IG.add_vertex cache i
  done;
  (* we already add the transitive closure on the fly *)
  (* let cg = Strongdeps_int.SO.O.add_transitive_closure cg in *)

  debug "dependency graph : nodes %d , edges %d" 
  (SG.nb_vertex cg) (SG.nb_edges cg);

  SG.iter_edges (IG.add_edge cache) cg;

  debug " done";

  let i = ref 0 in
  let ex = explicit mdf in
  let total = ref 0 in
  let conflict_size = List.length ex in

  let try_add_edge donei stronglist p q x y =
    incr donei;
    if not (IG.mem_edge cache p q) then begin
      IG.add_edge cache p q;
      let req = Diagnostic_int.Lst [p;q] in
      match Depsolver_int.solve solver req with
      |Diagnostic_int.Success _ -> ()
      |Diagnostic_int.Failure f ->
        CG.add_edge_e stronglist (p, (x, y, Other (f ())), q)
    end 
  in

  let debconf_triangle xpred ypred common =
    if not (S.is_empty common) then
      let xrest = S.diff xpred ypred
      and yrest = S.diff ypred xpred 
      and pred_pred = S.fold (fun z acc ->
        S.union closures.(z).rd acc
      ) common S.empty in
      S.subset xrest pred_pred && S.subset yrest pred_pred
    else
      false
  in

  (* The simplest algorithm. We iterate over all explicit conflicts, 
   * filtering out all couples that cannot possiby be in conflict
   * because either of strong dependencies or because already considered.
   * Then we iter over the reverse dependency closures of the selected 
   * conflict and we check all pairs that have not been considered before.
   * *)
  let stronglist = CG.create () in
  List.iter (fun (x,y) -> 
    incr i;
    if not(IG.mem_edge cache x y) then begin
      let pkg_x = index.(x) in
      let pkg_y = index.(y) in
      let (a,b) = (closures.(x).rdc, closures.(y).rdc) in 
      let donei = ref 0 in

      IG.add_edge cache x y;
      CG.add_edge_e stronglist (x, (x, y, Explicit), y);

      debug "(%d of %d) %s # %s ; Strong conflicts %d Tuples %d"
      !i conflict_size
      (pkg_x.Mdf.pkg.Cudf.package) 
      (pkg_y.Mdf.pkg.Cudf.package)
      (CG.nb_edges stronglist)
      ((S.cardinal a) * (S.cardinal b));

      Util.Progress.set_total localbar (S.cardinal a);
      List.iter (fun p ->
        List.iter (fun q ->
          if p <> q && not (IG.mem_edge cache p q) then
          begin
            IG.add_edge cache p q;
            CG.add_edge_e stronglist (p, (x, y, Conjunctive), q);
          end
        ) (y::(SG.pred cg y))
      ) (x::(SG.pred cg x));

      (* unless :
       * 1- x and y are in triangle, that is: there is ONE reverse dependency
       * of both x and y that has a disjunction "x | y". *)
      let xpred = closures.(x).rd
      and ypred = closures.(y).rd in 
      let common = S.inter xpred ypred in
      if (S.cardinal xpred = 1) && (S.cardinal ypred = 1) && (S.choose xpred = S.choose ypred) then
      begin
        let p = S.choose xpred in
        begin
          debug "triangle %s - %s (%s)" 
            (CudfAdd.print_package pkg_x.Mdf.pkg)
            (CudfAdd.print_package pkg_y.Mdf.pkg)
            (CudfAdd.print_package index.(p).Mdf.pkg);
          try_add_edge donei stronglist p x x y;
          try_add_edge donei stronglist p y x y;
        end
      end
      else if debconf_triangle xpred ypred common then
        debug "debconf triangle %s - %s"
          (CudfAdd.print_package pkg_x.Mdf.pkg)
          (CudfAdd.print_package pkg_y.Mdf.pkg)
      else
      begin
        S.iter (fun p ->
          S.iter (fun q ->
            try_add_edge donei stronglist p q x y 
          ) (S.diff b (to_set (IG.succ cache p))) ;
          Util.Progress.progress localbar;
        ) a
      end;

      Util.Progress.reset localbar;

      debug " | tuple examined %d" !donei;
      total := !total + !donei
    end
  ) ex ;
  debug " total tuple examined %d" !total;
  stronglist
;;
