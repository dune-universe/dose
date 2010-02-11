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

  let description = "Partition the dependency graph in clusters of coinstallable packages"
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
module GO = Defaultgraphs.GraphOper(UG)

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


let bk gr =
  let n v = N.set_from_vertex gr v in
  let rec fold f init s =
    if S.is_empty s then init
    else
      let v = S.choose s in
      fold f (f init v) (S.remove v s)
  in
  let rec aux1 acc r p x =
    if (S.is_empty p) && (S.is_empty x) then ([r] :: acc)
    else
      let u = S.choose (S.union p x) in
      let (_,_,_,mxc) = fold aux2 (r,p,x,[]) (S.diff p (n u))
      in mxc
  and aux2 (r,p,x,acc) v =
    let r' = S.union r (S.singleton v) in
    let p' = S.inter p (n v) in
    let x' = S.inter x (n v) in
    (r,S.remove v p, S.add v x, aux1 acc r' p' x')
  in
  let r = S.empty in
  let p = UG.fold_vertex S.add gr S.empty in
  let x = S.empty in
  aux1 [] r p x
;;

let rec bronKerbosch2 gr r p x =
  (* (n v) returns the set of neibourgh of v *)
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

let rec bronKerbosch2_ gr r p x =
  (* (n v) return the set of neibourgh of v *)
  let n v = N.set_from_vertex gr v in
  if (S.is_empty p) && (S.is_empty x) then return r
  else
    let u = S.choose (S.union p x) in
    let (_,_,mxc) =
      S.fold (fun v (p,x,acc) ->
        let r' = S.union r (S.singleton v) in
        let p' = S.inter p (n v) in
        let x' = S.inter x (n v) in
        (S.remove v p, S.add v x,mplus acc (bronKerbosch2_ gr r' p' x'))
      ) (S.diff p (n u)) (p,x,Enum.empty ())
    in mxc
;;

(*
   BronKerbosch2(R,P,X):
       if P and X are both empty:
           report R as a maximal clique
       choose a pivot vertex u in P ⋃ X
       for each vertex v in P \ N(u):
           BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
           P := P \ {v}
           X := X ⋃ {v}
*)

let bronKerbosch2__ gr r p x =
  let l = ref [] in
  let n v = N.set_from_vertex gr v in
  let rec aux r p x =
    if (S.is_empty p) && (S.is_empty x) then l := r::!l
    else
      let u = S.choose (S.union p x) in
      let pref = ref p in
      let xref = ref x in
      S.iter (fun v ->
        let r' = S.union r (S.singleton v) in
        let p' = S.inter !pref (n v) in
        let x' = S.inter !xref (n v) in
        aux r' p' x' ;
        pref := S.remove v !pref;
        xref := S.add v !xref
      ) (S.diff p (n u))
  in
  aux r p x;
  !l
;;

let max_independent_sets gr =
  let cgr = O.complement gr in
  let r = S.empty in
  let p = UG.fold_vertex S.add cgr S.empty in
  let x = S.empty in
  (* Printf.eprintf "p : %d%!" (S.cardinal p); *)
  bronKerbosch2_ cgr r p x
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

(** connected components. *)
(* XXX : not the most efficient/elegant way, isn't it ? 
    I should do a visit with marking and remove the hashtbl. *)
let connected_components graph =
  let module Dfs = Traverse.Dfs(UG) in
  let h = Hashtbl.create (UG.nb_vertex graph) in
  let l = ref [] in
  let cc graph id =
    let l = ref [] in
    let collect id = l := id :: !l in
    Dfs.prefix_component collect graph id;
    !l
  in
  UG.iter_vertex (fun v ->
    if not(Hashtbl.mem h v) then begin
      Hashtbl.add h v ();
      match cc graph v with
      |[] -> ()
      |c ->
          begin
            List.iter (fun x -> Hashtbl.add h x ()) c ;
            l := c :: !l
          end
    end
  ) graph ;
  !l

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
      with Not_found -> Hashtbl.add h p (ref [(Hashtbl.find ct v)])
    ) (rev_clo v)
  ) cg
  ;
  let l = Hashtbl.fold (fun v k acc -> (v,k)::acc ) h [] in
  List.sort ~cmp:(fun (_,k1) (_,k2) -> (List.length !k1) - (List.length !k2)) l 
;;

exception Forall
let for_all f e = 
  if Enum.is_empty e then true
  else
    try (Enum.iter (fun a -> if f a then () else raise Forall) e ; true)
    with Forall -> false

let filter_map f e =
  let rec aux acc f e =
    match Enum.get e with
    |None -> acc
    |Some s ->
      match f s with
      |Some s -> aux (s::acc) f e
      |None -> aux acc f e
  in
  match aux [] f e with
  |[] -> assert false
  |l -> List.enum l

let install solver _ ll = 
  let aux l = 
    Depsolver_int.S.reset solver.Depsolver_int.constraints;
    match Depsolver_int.solve solver (Diagnostic_int.Lst l) with
    |Diagnostic_int.Failure _ -> false
    |Diagnostic_int.Success _ -> true
  in
  let s = List.fold_left S.union S.empty ll in
  (* Printf.eprintf "--%!"; *)
  let l = S.elements s in
  aux l
;;

let rec subsetsk k = function
  |[] -> return []
  |h :: t ->
      bind (subsetsk k t) (fun t1 ->
        mplus (
          bind (return t1) (fun t2 -> 
            bind (guard (card(t2) = k-1)) (fun _ -> return (h :: t2))
          )) (return t1)
      )
;;

let rec subsets = function
  |[] -> return []
  |h1 :: t -> Enum.append (List.enum (List.map (fun h2 -> [h1;h2]) t)) (subsets t)

let rec allsets m l =
  if m = 0 then return [] else
  match l with
  |[] -> return []
  |h::t -> Enum.append (Enum.map (fun g -> h::g) (allsets (m - 1) t)) (allsets m t)
;;

let rec allsubsets = function
  |[] -> [[]]
  |a::t -> 
      let res = allsubsets t in
      List.map (fun b -> a::b) res @ res
;;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b

(*
# cartesian_product [1;2;3] [3;4];;
- : (int * int) list = [(1, 3); (1, 4); (2, 3); (2, 4); (3, 3); (3, 4)]
*)
let cartesian l1 l2 = bind l1 (fun x -> bind l2 (fun y -> return (x,y))) ;;

(* let map = List.map *)
let rec permutation = function 
  |[] -> return []
  |h::t ->
      bind (permutation t) (fun t1 ->
        Enum.map (fun h1 -> h1 :: t1) h
      )

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
  (*
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
  *)
  (*
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
  *)
  let a = Array.make (Array.length index) false in
  (* add an arch between two cc if there is a package that eliminates a cross
   * model. Problem : if the coverage is not complete I can never be sure that
   * such package does not exists. The vertex of the resulting graph are the cc
   * that do not partition the packages space. The complementary graph
   * represents all the cc that are independent *)
  (* let g = UG.create () in *)
  let hard = ref [] in
  let c = ref 0 in
  List.iter (fun (p,ll) ->
    incr c;
    let size = List.length !ll in
    Printf.printf "(%d or %d) package %s (%d) %!" !c (List.length pt) 
    (CudfAdd.print_package (maps.CudfAdd.map#inttovar p)) size ;
    let r =
      List.for_all (fun m ->
        for_all (fun l ->
          (* Printf.eprintf "\nAll sets of size %d (%d)%!" m (List.length l); *)
          let gl = List.map (fun xl -> (filter cg !xl)) l in
          if List.exists (fun g -> UG.nb_vertex g > 30) gl then
            (hard := (p,ll) :: !hard ; false)
          else begin
            let sgl =
              List.sort ~cmp:(fun c1 c2 -> (UG.nb_vertex c1) - (UG.nb_vertex c2)) gl
            in
            let misl =
              List.map (fun g ->
                (* Printf.eprintf "@%!"; *)
                let e = 
                  filter_map (fun s ->
                    if install solver a [s] then Some s else None
                  ) (max_independent_sets g)
                in if Enum.is_empty e then assert false else e
              ) sgl
            in
            (* Printf.eprintf ">>%!"; *)
            for_all (fun e -> 
              (* Printf.eprintf ".%!"; *)
              install solver a ((S.singleton p)::e)
            ) (permutation misl)
          end
        ) (allsets m !ll)
      ) (List.rev (range 1 size))
    in
    a.(p) <- r ;
    (if r then Printf.printf "is always installable\n%!"
    else Printf.printf "eliminates at least one mis\n%!")
  ) pt 
  ;
  let elim = Array.fold_left (fun acc v -> if v then acc + 1 else acc) 0 a in
  Printf.eprintf "Total : %d , hard : %d , elim : %d , notelim : %d\n" 
  (List.length pt) (List.length !hard) elim ((List.length pt) - (List.length !hard) - elim)
 ;;

main () ;;
