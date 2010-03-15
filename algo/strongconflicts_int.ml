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

module SG = Strongdeps_int.G
module PkgV = struct
    type t = int
    let compare = Pervasives.compare
    let hash i = i
    let equal = (=)
end
(* unlabelled indirected graph *)
module IG = Graph.Imperative.Graph.Concrete(PkgV)

(** progress bar *)
let seedingbar = Util.Progress.create "Algo.Strongconflicts.seeding" ;;
let localbar = Util.Progress.create "Algo.Strongconflicts.local" ;;

open Depsolver_int

module S = Set.Make (struct type t = int let compare = Pervasives.compare end)
type cl = { rdc : S.t ; rd : S.t } 

let swap (p,q) = if p < q then (p,q) else (q,p) ;;
let to_set l = List.fold_right S.add l S.empty ;;

let coinst_partial solver (p,q) =
  let req = Diagnostic_int.Lst [p;q] in
  match Depsolver_int.solve solver req with
  |Diagnostic_int.Success _ -> true
  |Diagnostic_int.Failure _ -> false
;;

let explicit mdf =
  let index = mdf.Mdf.index in
  let l = ref [] in
  for i=0 to (Array.length index - 1) do
    let pkg = index.(i) in
    let conflicts = Array.map snd pkg.Mdf.conflicts in
    for j=0 to ((Array.length conflicts) - 1) do
      l := swap(i,conflicts.(j)):: !l
    done
  done;
  (List.unique !l)
;;

(* [strongconflicts mdf idlist] return the list of strong conflicts
   @param mdf
   @param a subset of the universe
*)
let strongconflicts mdf idlist =
  let index = mdf.Mdf.index in
  let clousure = Depsolver_int.dependency_closure mdf idlist in
  let solver = Depsolver_int.init_solver ~closure index in
  let coinst = coinst_partial solver in
  let reverse = reverse_dependencies mdf in
  let size = (List.length idlist) in
  let cache = IG.create ~size:size () in
  let cl_dummy = {rdc = S.empty ; rd = S.empty} in
  let closures = Array.create size cl_dummy in

  let triangles x y =
    let s1 = closures.(x).rd in
    let s2 = closures.(y).rd in
    (S.is_empty s1 && S.is_empty s2) || (
    (S.equal s1 s2) &&
    (List.for_all (fun e ->
      (List.exists (fun (_,a,_) ->
        Array.mem x a && Array.mem y a
      ) (Array.to_list index.(e).Mdf.depends)) &&
      (coinst (e,x) && coinst (e,y))
    ) (S.elements s1))
    )
  in

  Util.print_info "Pre-seeding ...";

  Util.Progress.set_total seedingbar (List.length idlist);

  let cg = SG.create ~size:(List.length idlist) () in
  for i=0 to (size - 1) do
    Util.Progress.progress seedingbar;
    let rdc = to_set (reverse_dependency_closure reverse [i]) in
    let rd = to_set reverse.(i) in
    closures.(i) <- {rdc = rdc; rd = rd};
    Strongdeps_int.conjdepgraph_int cg index i ; 
    IG.add_vertex cache i
  done;
  let cg = Strongdeps_int.SO.O.add_transitive_closure cg in

  Util.print_info "dependency graph : nodes %d , edges %d" 
  (SG.nb_vertex cg) (SG.nb_edges cg);

  SG.iter_edges (IG.add_edge cache) cg;

  Util.print_info " done";

  let i = ref 0 in
  let ex = explicit mdf in
  let total = ref 0 in
  let conflict_size = List.length ex in

  (* The simplest algorithm. We iterate over all explicit conflicts, 
   * filtering out all couples that cannot possiby be in conflict
   * because either of strong dependencies or because already considered.
   * Then we iter over the reverse dependency closures of the selected 
   * conflict and we check all pairs that have not been considered before.
   * *)
  let stronglist = IG.create () in
  List.iter (fun (x,y) -> 
    incr i;
    if not(IG.mem_edge cache x y) then begin
      let pkg_x = index.(x) in
      let pkg_y = index.(y) in
      let (a,b) = (closures.(x).rdc, closures.(y).rdc) in 
      let donei = ref 0 in

      IG.add_edge cache x y;
      IG.add_edge stronglist x y;

      Util.print_info "(%d of %d) %s # %s ; Strong conflicts %d Tuples %d"
      !i conflict_size
      (CudfAdd.print_package pkg_x.Mdf.pkg) 
      (CudfAdd.print_package pkg_y.Mdf.pkg)
      (IG.nb_edges stronglist)
      ((S.cardinal a) * (S.cardinal b));

      Util.Progress.set_total localbar (S.cardinal a);

      (* unless :
       * 1- x and y are in triangle, that is all direct reverse dependencies have a
       * disjunction containing x and y and it is always possible to install
       * either x or y. or the direct reverse dependencies of x and y are empty.
       * 2- the reverse dependency clousure of x and y are equal
       * 
       * -> check the product A x B
       * *)
      if (triangles x y) || (S.equal a b) then ()
      else begin
        S.iter (fun p ->
          S.iter (fun q ->
            incr donei;
            if not (IG.mem_edge cache p q) then begin
              if not (coinst (p,q)) then 
                IG.add_edge stronglist p q;
            end ;
            IG.add_edge cache p q;
          ) (S.diff b (to_set (IG.succ cache p))) ;
          Util.Progress.progress localbar;
        ) a
      end;

      Util.Progress.reset localbar;

      Util.print_info " | tuple examined %d" !donei;
      total := !total + !donei
    end
  ) ex ;
  Util.print_info " total tuple examined %d" !total;
  IG.fold_edges (fun p q l -> swap(p,q) :: l) stronglist []
;;
