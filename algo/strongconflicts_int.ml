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

module ST = Set.Make (struct type t = (int * int) let compare = Pervasives.compare end)
module S = Set.Make (struct type t = int let compare = Pervasives.compare end)
type cl = { rdc : S.t ; rd : S.t } 

let impactset graph q =
  if SG.mem_vertex graph q then
    SG.fold_pred (fun p acc -> S.add p acc) graph q S.empty
  else S.empty

let strongset graph q =
  if SG.mem_vertex graph q then
    SG.fold_succ (fun p acc -> S.add p acc) graph q S.empty
  else S.empty

let swap (p,q) = if p < q then (p,q) else (q,p) ;;


let coinst_partial_list solver l =
  let req = Diagnostic_int.Lst l in
  match Depsolver_int.solve solver req with
  |Diagnostic_int.Success _ -> true
  |Diagnostic_int.Failure _ -> false
;;

let coinst_partial solver (p,q) = coinst_partial_list solver [p;q] ;;

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

class cache size = object
  val data = Array.make size S.empty

  method init g i =
    let is = impactset g i in
    let ss = strongset g i in
    data.(i) <- S.union is ss;

  method add p q =
    data.(q) <- S.add p data.(q);
    data.(p) <- S.add q data.(p)

  method get p = data.(p)

  method mem p q = 
    (S.mem q data.(p)) || (S.mem p data.(q))

end

let strongconflicts mdf idlist =
  let index = mdf.Mdf.index in
  let solver = init_solver ~idlist index in
  let coinst = coinst_partial solver in
  let reverse = reverse_dependencies mdf in
  let size = (List.length idlist) in
  let cacheobj = new cache size in
  let cl_dummy = {rdc = S.empty ; rd = S.empty} in
  let closures = Array.create size cl_dummy in
  let to_set l = List.fold_right S.add l S.empty in

  let triangles x y =
    let s1 = closures.(x).rd in
    let s2 = closures.(y).rd in
    (S.equal s1 s2) &&
    (List.for_all (fun e ->
      List.exists (fun (_,a,_) ->
        Array.mem x a && Array.mem y a
      ) (Array.to_list index.(e).Mdf.depends)
    ) (S.elements s1)) &&
    (List.for_all (fun e ->
      coinst (e,x) && coinst (e,y)
    ) (S.elements s1))
    (*
    (coinst_partial_list solver (x::(S.elements s1))) &&
    (coinst_partial_list solver (y::(S.elements s1))) *)

    (* (S.cardinal s1 = 1) *)
(*    (S.equal s1 s2) &&
    List.for_all (fun e ->
      List.exists (fun (_,a,_) ->
        Array.mem x a && Array.mem y a
      ) (Array.to_list index.(e).Mdf.depends)
    ) (S.elements s1)
*)
  in

  Util.print_info "Pre-seeding ...";

  Util.Progress.set_total seedingbar (List.length idlist);
  let cg = Strongdeps_int.G.create ~size:(List.length idlist) () in
  (* let cg = Strongdeps_int.strongdeps mdf idlist in *)
  for i=0 to (size - 1) do
    Util.Progress.progress seedingbar;
    let rdc = to_set (reverse_dependency_closure reverse [i]) in
    let rd = to_set reverse.(i) in
    closures.(i) <- {rdc = rdc; rd = rd};
    Strongdeps_int.conjdepgraph_int cg index i ; 
  done;
  let cg = Strongdeps_int.SO.O.add_transitive_closure cg in
  Util.print_info "dependency graph : nodes %d , edges %d" 
  (SG.nb_vertex cg) (SG.nb_edges cg);
  for i=0 to (size - 1) do cacheobj#init cg i done;
  Util.print_info " done";

  let i = ref 0 in

  let cmp (x1,y1) (x2,y2) =
    let (a1,b1) = (closures.(x1).rdc, closures.(y1).rdc) in
    let (a2,b2) = (closures.(x2).rdc, closures.(y2).rdc) in
    ( ((S.cardinal a1) * (S.cardinal b1)) - ((S.cardinal a2) * (S.cardinal b2)))
  in

  let ex = List.unique (List.sort ~cmp (explicit mdf)) in
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
    if not(cacheobj#mem x y) then begin
      let pkg_x = index.(x) in
      let pkg_y = index.(y) in
      let (a,b) = (closures.(x).rdc, closures.(y).rdc) in 
(*      let (a,b) = if S.cardinal a < S.cardinal b then (a,b) else (b,a) in *)
      let donei = ref 0 in

      cacheobj#add x y;
      IG.add_edge stronglist x y;
(*      SG.iter_pred (fun v ->
        IG.add_edge stronglist x v;
        cacheobj#add x v
      ) cg y;
      SG.iter_pred (fun v ->
        IG.add_edge stronglist y v;
        cacheobj#add y v
      ) cg x;
*)

      Util.print_info "(%d of %d) %s # %s ; Strong conflicts %d Tuples %d"
      !i conflict_size
      (CudfAdd.print_package pkg_x.Mdf.pkg) 
      (CudfAdd.print_package pkg_y.Mdf.pkg)
      (IG.nb_edges stronglist)
      ((S.cardinal a) * (S.cardinal b));

      Util.Progress.set_total localbar (S.cardinal a);

      let s1 = closures.(x).rd in
      let s2 = closures.(y).rd in

      Printf.printf "%s # %s\n"
      (CudfAdd.print_package pkg_x.Mdf.pkg)
      (CudfAdd.print_package pkg_y.Mdf.pkg);

      Printf.printf "x : %d\n" x;
      Printf.printf "y : %d\n" y;

      Printf.printf "s1 : %!"; S.iter (Printf.printf "%d %!") s1; Printf.printf "\n%!";
      Printf.printf "s2 : %!"; S.iter (Printf.printf "%d %!") s2; Printf.printf "\n%!";

      Printf.printf "a : %!"; S.iter (Printf.printf "%d %!") a; Printf.printf "\n%!";
      Printf.printf "b : %!"; S.iter (Printf.printf "%d %!") b; Printf.printf "\n%!";

      (* useless to check if *)
      if (S.is_empty closures.(x).rd && S.is_empty closures.(y).rd) ||
         (triangles x y) ||
         (S.equal a b)
      then ()
      else begin
        S.iter (fun p ->
          S.iter (fun q ->
            incr donei;
            if not (cacheobj#mem p q) then begin
              if not (coinst (p,q)) then 
                IG.add_edge stronglist p q;
            end ;
            cacheobj#add p q;
          ) (S.diff b (cacheobj#get p)) ;
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

(*
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
    else if S.equal closures.(x).rdc closures.(y).rdc then false else true
      (* begin
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
    end *)
  in

  let triangles mdf x y = 
    if S.equal closures.(x).rd closures.(y).rd then false
    else
      if S.equal closures.(x).rdc closures.(y).rdc then false
      else true
  in

*)
