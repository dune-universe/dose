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

(** progress bar *)
let seeding = Util.Progress.create "Algo.Strongconflicts.seeding" ;;
let mainbar = Util.Progress.create "Algo.Strongconflicts.main" ;;

open Depsolver_int

module ST = Set.Make (struct type t = (int * int) let compare = Pervasives.compare end)
module S = Set.Make (struct type t = int let compare = Pervasives.compare end)
type cl = { rdc : S.t ; impactset : S.t} 

let impactset graph q =
  if SG.mem_vertex graph q then
    SG.fold_pred (fun p acc -> S.add p acc) graph q S.empty
  else S.empty

let strongset graph q =
  if SG.mem_vertex graph q then
    SG.fold_succ (fun p acc -> S.add p acc) graph q S.empty
  else S.empty

let swap (p,q) = if p < q then (p,q) else (q,p) ;;

let addstrong closures s (p,q) =
  let isp = S.add p closures.(p).impactset in
  let isq = S.add q closures.(q).impactset in
  let res = 
    S.fold (fun a acc ->
      S.fold (fun b acc ->
        ST.add (swap (a,b)) acc
      ) isp acc
    ) isq !s
  in s := res
;;

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
  let result = ref ST.empty in
  let coinst = coinst_partial solver in
  let reverse = reverse_dependencies mdf in
  let size = (List.length idlist) in
  let cache = Array.create size S.empty in
  let cl_dummy = {rdc = S.empty ; impactset = S.empty} in
  let closures = Array.create size cl_dummy in
  let to_set l = List.fold_right S.add l S.empty in

  Printf.eprintf "Pre-seeding ...%!\n";
  Util.Progress.set_total seeding (List.length idlist);
  for i=0 to (size - 1) do
    Util.Progress.progress seeding;
    let rdc = to_set (reverse_dependency_closure reverse [i]) in
    let is = impactset sdgraph i in
    let ss = strongset sdgraph i in
    closures.(i) <- {rdc = rdc; impactset = is};
    cache.(i) <- S.union is ss;
  done;

  let i = ref 0 in

  let cmp (x1,y1) (x2,y2) =
    let (a1,b1) = (closures.(x1).rdc, closures.(y1).rdc) in
    let (a2,b2) = (closures.(x2).rdc, closures.(y2).rdc) in
    ((S.cardinal a1) * (S.cardinal b1)) - ((S.cardinal a2) * (S.cardinal b2))
  in

  let ex = List.unique (List.sort ~cmp (explicit mdf)) in
  let total = ref 0 in
  let conflict_size = List.length ex in

  List.iter (fun (x,y) -> 
    if not((S.mem x cache.(y)) || (S.mem y cache.(x))) then begin
      incr i;
      let pkg_x = index.(x) in
      let pkg_y = index.(y) in
      let (a,b) = (closures.(x).rdc, closures.(y).rdc) in 
      let (a,b) = if S.cardinal a < S.cardinal b then (a,b) else (b,a) in
      let donei = ref 0 in

      Printf.eprintf "(%d of %d) %s # %s ; Strong conflicts %d Tuples %d %!\n"
      !i conflict_size
      (CudfAdd.print_package pkg_x.Mdf.pkg) 
      (CudfAdd.print_package pkg_y.Mdf.pkg)
      (ST.cardinal !result)
      ((S.cardinal a) * (S.cardinal b));

      addstrong closures result (x,y);
      cache.(x) <- S.add y cache.(x) ;
      cache.(y) <- S.add x cache.(y) ;

      Util.Progress.set_total mainbar (S.cardinal a);

      (* debconf-i18n | debconf-english problem ? *)
      if not (S.equal a b) &&
      not (S.equal (to_set reverse.(x)) (to_set reverse.(y))) then begin
        S.iter (fun p ->
          Util.Progress.progress mainbar;
          S.iter (fun q ->
            if not ((S.mem p cache.(q)) || (S.mem q cache.(p))) then begin
              incr donei;
              if not (coinst (p,q)) then
                addstrong closures result (p,q);
            end
            ;
            cache.(q) <- S.add p cache.(q) ;
            cache.(p) <- S.add q cache.(p)
          ) (S.diff b cache.(p)) ;
        ) a
       end ;

      Util.Progress.reset mainbar;

      Printf.eprintf " | tuple examined %d\n%!" !donei;
      total := !total + !donei
    end
  ) ex ;

  Printf.eprintf " total tuple examined %d\n%!" !total;
  ST.elements !result
;;
