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

open Cudf
open ExtLib
open Common
open Algo
open Graph

module Options =
struct
  open OptParse
  let debug = StdOpt.store_true ()

  let description = "Ceve ... what does it mean ?"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~long_name:"debug" ~help:"Print debug information" debug;
end

(* XXX to refactor in Borilerplate.ml *)
let parse uri =
  Printf.eprintf "Parsing and normalizing...%!" ;
  let timer = Common.Util.Timer.create "Parsing and normalizing" in
  Common.Util.Timer.start timer;
  let pkglist =
    match Input.parse_uri uri with
    |("deb",(_,_,_,_,file),_) -> begin
      let l = Debian.Packages.input_raw [file] in
      let tables = Debian.Debcudf.init_tables l in
      List.map (Debian.Debcudf.tocudf tables) l
    end
    |("cudf",(_,_,_,_,file),_) -> begin
      let _, l, _ = CudfAdd.parse_cudf file in l
    end
    |_ -> failwith "Not supported"
  in
  ignore(Common.Util.Timer.stop timer ());
  Printf.eprintf "done\n%!" ;
  pkglist
;;

(* ----------------------------------- *)

module PkgV = struct
    type t = int
    let compare = Pervasives.compare
    let hash i = i
    let equal = (=)
end
(* unlabelled indirected graph *)
(* module UG = Persistent.Graph.Concrete(PkgV) *)
module UG = Imperative.Graph.Concrete(PkgV)
module N = Oper.Neighbourhood(UG)
module O = Oper.Make(Builder.I(UG))
module S = N.Vertex_Set

let rec bronKerbosch2 gr r p x =
  let n v = N.set_from_vertex gr v in
  if (S.is_empty p) && (S.is_empty x) then [r]
  else
    let u = S.choose (S.union p x) in
    let (_,_,mxc) =
      S.fold (fun v (p,x,acc) ->
        let r' = S.union r (S.singleton v) in
        let p' = S.inter p (n v) in
        let x' = S.inter x (n v) in
        (S.remove v p, S.add v x,(bronKerbosch2 gr r' p' x') @ acc)
      ) (S.diff p (n u)) (p,x,[])
    in mxc
;;

let max_independent_sets gr =
  let cgr = O.complement gr in
  let r = S.empty in
  let p = UG.fold_vertex S.add cgr S.empty in
  let x = S.empty in
  bronKerbosch2 cgr r p x
;;

let conflictgraph mdf =
  let index = mdf.Mdf.index in
  let g =UG.create () in
  for i=0 to (Array.length index - 1) do
    let pkg = index.(i) in
    let conflicts = Array.map snd pkg.Mdf.conflicts in
    for j=0 to ((Array.length conflicts) - 1) do
      UG.add_edge g i conflicts.(j)
    done
  done;
  g
;;

let filter gr cc =
  let g = UG.create () in
  List.iter (fun v1 ->
    UG.iter_succ (fun v2 ->
      UG.add_edge g v1 v2
    ) gr v1
  ) cc
  ;
  g
;;

let connected_components g =
  let h = Hashtbl.create (UG.nb_vertex g) in
  let l = ref [] in
  let cc graph id =
    let module Dfs = Traverse.Dfs(UG) in
    let l = ref [] in
    let collect id = l := id :: !l in
    Dfs.prefix_component collect graph id;
    !l
  in
  UG.iter_vertex (fun v ->
    if not(Hashtbl.mem h v) then begin
      let c = cc g v in
      List.iter (fun x -> Hashtbl.add h x ()) c ;
      l := c :: !l
    end
  ) g ;
  !l
;;

(* associate a connected component to each conflict node *)
let conflict_table cc =
  let h = Hashtbl.create (2 * List.length cc) in
  List.iter (fun l ->
    List.iter (fun v ->
      Hashtbl.add h v (ref l)
    ) l
  ) cc
  ;
  h
;;

(* associate a list of connected components to each package *)
let package_table reverse cg ct =
  let h = Hashtbl.create (UG.nb_vertex cg) in
  let rev_clo pid = Depsolver_int.reverse_dependency_closure reverse [pid] in
  UG.iter_vertex (fun v ->
    List.iter (fun p ->
      try
        let l = Hashtbl.find h p in
        try l := (Hashtbl.find ct v)::!l
        with Not_found -> assert false
      with Not_found -> Hashtbl.add h p (ref [])
    ) (rev_clo v)
  ) cg
  ;
  h
;;

exception Forall
let for_all f e = 
  try Enum.iter (fun a -> if f a then () else raise Forall) e ; true
  with Forall -> false

let install solver p ll = 
  for_all (fun s ->
    let l = S.elements s in
    Depsolver_int.S.reset solver.Depsolver_int.constraints;
    match Depsolver_int.solve solver (Diagnostic_int.Lst (p::l)) with
    |Diagnostic_int.Failure _ -> false
    |Diagnostic_int.Success _ -> true
  ) ll
;;

(*
let return a = [a]
let bind m f = List.flatten (List.map f m)
let mzero = []
let guard b = if b then return () else mzero
let card l = (List.length l)
let mplus = List.append
*)

let return a = let e = Enum.empty () in Enum.push e a ; e
let bind m f = Enum.concat (Enum.map f m)
let mzero = Enum.empty ()
let guard b = if b then return () else mzero
let mplus = Enum.append
let card l = (List.length l)

let rec subsets = function
  |[] -> return []
  |h :: t ->
      bind (subsets t) (fun t1 ->
        mplus (
          bind (return t1) (fun t2 -> return (h :: t2))
        ) (return t1)
      )
;;

(* all subsets with cardinality less then k *)
(* [ x | x <- (subsets X) ; |x| <= k ] *)
(*
# subsets_k 1 [1;2];;                      
- : int list list = [[2]; [1]; []]

*)
let subsets_k k l =
  bind (subsets l) (fun x ->
    bind (guard (card(x) <= k)) (fun _ ->
      return x
    )
  )
;;

(*
# cartesian_product [1;2;3] [3;4];;
- : (int * int) list = [(1, 3); (1, 4); (2, 3); (2, 4); (3, 3); (3, 4)]
*)
let cartesian l1 l2 =
  bind l1 (fun x ->
    bind l2 (fun y ->
      return (x,y)
    )
  )
;;

let cmp l1 l2 = (List.length l2) - (List.length l1)

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  let pkglist = match posargs with [uri] -> parse uri | _ -> assert false in
  let mdf = Mdf.load_from_list pkglist in
  let maps = mdf.Mdf.maps in
  let index = mdf.Mdf.index in
  let solver = Depsolver_int.init_solver index in
  let reverse = Depsolver_int.reverse_dependencies mdf in
  let cg = conflictgraph mdf in
  let cc = connected_components cg in
  Printf.eprintf "conflict graph = vertex : %d , edges : %d\n"
  (UG.nb_vertex cg)(UG.nb_edges cg);
  Printf.eprintf "connected components = n. %d , largest : %d\n"
  (List.length cc) (List.length (List.hd (List.sort ~cmp:cmp cc)));
  let ct = conflict_table cc in
  (* consider only packages that depends on conflicts *)
  let pt = package_table reverse cg ct in
  let maxc = ref 0 in
  let sumc = ref 0 in
  Hashtbl.iter (fun p l ->
    let m = (List.length !l) in
    if m > !maxc then maxc := m;
    sumc := m + !sumc;
    (* Printf.eprintf "%s : %d\n" (CudfAdd.print_package (maps.CudfAdd.map#inttovar p)) m *)
  ) pt
  ;
  Printf.eprintf "Conflicts in the cone = largest : %d , avg : %d\n" !maxc (!sumc / (Hashtbl.length pt));
  let maxc = ref 0 in
  List.iter (fun c ->
    if List.length c < 30 then begin
      (* Printf.eprintf "c = %d%!" (List.length c); *)
      let mis = max_independent_sets (filter cg c) in
      (* Printf.eprintf "mis = %d\n%!" (List.length mis); *)
      let m = (List.length mis) in
      if m > !maxc then maxc := m;
    end
      else
        Printf.eprintf "Skip %d\n" (List.length c)
  ) cc
  ;
  Printf.eprintf "Maximal independent sets = largest : %d\n%!" !maxc;
  let ignored = ref 0 in
  let elim = ref 0 in
  let notelim = ref 0 in
  (* add an arch between two cc if there is a package that eliminates a cross
   * model. Problem : if the coverage is not complete I can never be sure that
   * such package does not exists. The vertex of the resulting graph are the cc
   * that do not partition the packages space. The complementary graph
   * represents all the cc that are independent *)
  let g = UG.create () in
  Hashtbl.iter (fun p ll ->
    Printf.eprintf "package %s %!" (CudfAdd.print_package (maps.CudfAdd.map#inttovar p)) ;
    if List.for_all (fun xl -> List.length !xl < 30 ) !ll then begin
      if List.for_all (fun xl ->
        Printf.eprintf "(vars : %d) %!" (List.length !xl) ;
        let mis = max_independent_sets (filter cg !xl) in
        Printf.eprintf "(mis : %d) %!" (List.length mis) ; 
        install solver p (List.enum mis)
      ) !ll
      then begin
        let l = List.map (fun xl -> max_independent_sets (filter cg !xl)) !ll in 
        Printf.eprintf "(misl : %d) %!" (List.length l);
        if (List.length l) < 28 then begin
          try 
            Enum.iter (function
              |[m1;m2] -> 
                  Printf.eprintf "(cp vars : %d) %!" ((List.length m1) * (List.length m2));
                  let mis1 = List.enum m1 in
                  let mis2 = List.enum m2 in
                  let cp = Enum.map (fun (m1,m2) -> Printf.eprintf ".%!"; (S.union m1 m2)) (cartesian mis1 mis2) in 
                  if not(install solver p cp) then raise Forall
              |_ -> ()
            ) (subsets_k 2 l);
            (incr notelim ; Printf.eprintf "notelim \n%!")
          with Forall -> (incr elim ; Printf.eprintf "elim \n%!")
        end
        else (incr ignored ; Printf.eprintf "ignored \n%!")
      end
      else (incr elim ; Printf.eprintf "elim \n%!")
    end
    else (incr ignored ; Printf.eprintf "ignored \n%!")
  ) pt 
  ;
  Printf.eprintf "Coverage : %d , ignored : %d , elim : %d , notelim : %d , total : %d\n" 
  (!ignored + !elim + !notelim) !ignored !elim !notelim (Hashtbl.length pt)
;;

main () ;;
