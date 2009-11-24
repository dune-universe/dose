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

(** Strong Conflicts *)

open Graph
open ExtLib
open Common
open CudfAdd

module SG = Strongdeps_int.G

(* we use an undirected graph as cache *)
module PkgV = struct
    type t = int
    let compare = Pervasives.compare
    let hash i = i
    let equal = (=)
end
module UG = Graph.Imperative.Graph.Concrete(PkgV)

(** progress bar *)
let seedingbar = Util.Progress.create "Algo.Strongconflicts.seeding" ;;
let localbar = Util.Progress.create "Algo.Strongconflicts.local" ;;

open Depsolver_int

module ST = Set.Make (struct type t = (int * int) let compare = Pervasives.compare end)
module S = Set.Make (struct type t = int let compare = Pervasives.compare end)
type cl = { rdc : S.t ; rd : S.t } 

let swap (p,q) = if p < q then (p,q) else (q,p) ;;

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
  !l

let strongconflicts sdgraph mdf idlist =
  let index = mdf.Mdf.index in
  let solver = init_solver ~idlist index in
  let coinst = coinst_partial solver in
  let reverse = reverse_dependencies mdf in
  let size = (List.length idlist) in
  let cachegraph = UG.create ~size:(SG.nb_vertex sdgraph) () in
  let cl_dummy = {rdc = S.empty ; rd = S.empty} in
  let closures = Array.create size cl_dummy in
  let to_set l = List.fold_right S.add l S.empty in

  Util.print_info "Pre-seeding ...";
  Util.Progress.set_total seedingbar (List.length idlist);
  for i=0 to (size - 1) do
    Util.Progress.progress seedingbar;
    let rdc = to_set (reverse_dependency_closure reverse [i]) in
    let rd = to_set reverse.(i) in
    closures.(i) <- {rdc = rdc; rd = rd};
  done;
  SG.iter_edges (UG.add_edge cachegraph) sdgraph ;
  (* we add all vertex so to avoid succ exceptions *)
  SG.iter_vertex (UG.add_vertex cachegraph) sdgraph ;
  Util.print_info " done";

  let i = ref 0 in

  let cmp (x1,y1) (x2,y2) =
    let (a1,b1) = (closures.(x1).rdc, closures.(y1).rdc) in
    let (a2,b2) = (closures.(x2).rdc, closures.(y2).rdc) in
    ((S.cardinal a1) * (S.cardinal b1)) - ((S.cardinal a2) * (S.cardinal b2))
  in

  let ex = List.unique (List.sort ~cmp (explicit mdf)) in
  let total = ref 0 in
  let conflict_size = List.length ex in

  (* either all predecessor are the same or there exists a 
   * predecessor in the intersection that can however always
   * choose between x and y *)
  let triangles mdf x y =
    let s1 = closures.(x).rd in
    let s2 = closures.(y).rd in
    if S.equal s1 s2 then  
      (* we can always choose either x or y *)
      if List.for_all (fun e ->
        List.exists (fun (_,a,_) ->
          Array.mem x a && Array.mem y a
        ) (Array.to_list index.(e).Mdf.depends)
      ) (S.elements s1) then false
      (* common predecessors, but no x,y disjunctions *)
      else true 
    else (* begin
      (* all common predecessors *)
      let inter = S.inter s1 s2 in
      (* predecessors that are not common *)
      let s = S.diff (S.union s1 s2) inter in
      (* all non-common predecessors can be installed with both x and y,
       * meaning that I can always choose one of them *)
      if List.for_all (fun e ->
        let dc = Depsolver_int.dependency_closure ~maxdepth:2 mdf [e] in
        List.mem x dc && List.mem y dc && coinst(x,e) && coinst(y,e)
        ) (S.elements s) then false
      else true
    end *) true
  in

  (* The simplest algorithm. We iterate over all explicit conflicts, 
   * filtering out all couples that cannot possiby be in conflict
   * because either of strong dependencies or because already considered.
   * Then we iter over the reverse dependency closures of the selected 
   * conflict and we check all pairs that have not been considered before.
   * *)
  (* let stronglist = ref [] in *)
  let stronglist = UG.create () in
  List.iter (fun (x,y) -> 
    incr i;
    UG.add_edge stronglist x y;
    if not(UG.mem_edge cachegraph x y) then begin
      let pkg_x = index.(x) in
      let pkg_y = index.(y) in
      let (a,b) = (closures.(x).rdc, closures.(y).rdc) in 
      let (a,b) = if S.cardinal a < S.cardinal b then (a,b) else (b,a) in
      let donei = ref 0 in

      Util.print_info "(%d of %d) %s # %s ; Strong conflicts %d Tuples %d"
      !i conflict_size
      (CudfAdd.print_package pkg_x.Mdf.pkg) 
      (CudfAdd.print_package pkg_y.Mdf.pkg)
      (UG.nb_edges stronglist)
      ((S.cardinal a) * (S.cardinal b));

      UG.add_edge cachegraph x y;

      Util.Progress.set_total localbar (S.cardinal a);

      if triangles mdf x y then begin
        S.iter (fun p ->
          S.iter (fun q ->
            incr donei;
            if not (UG.mem_edge cachegraph p q) then begin
              UG.add_edge cachegraph p q;
              if not (coinst (p,q)) then 
                UG.add_edge stronglist p q;
            end
          ) (S.diff b (to_set (UG.succ cachegraph p))) ;
          Util.Progress.progress localbar;
        ) a
       end ;

      Util.Progress.reset localbar;

      Util.print_info " | tuple examined %d" !donei;
      total := !total + !donei
    end
  ) ex ;

  Util.print_info " total tuple examined %d" !total;
  UG.fold_edges (fun p q l -> swap(p,q) :: l) stronglist []
;;
