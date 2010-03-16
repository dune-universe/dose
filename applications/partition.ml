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
(* open Graph *)

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
module UG = Graph.Imperative.Graph.Concrete(PkgV)
module N = Graph.Oper.Neighbourhood(UG)
module O = Graph.Oper.Make(Graph.Builder.I(UG))
module S = N.Vertex_Set
module GO = Defaultgraphs.GraphOper(UG)

type 'a llist = 'a cell Lazy.t
and 'a cell = LList of 'a * 'a llist | Empty

exception LListEmpty

let empty = lazy(Empty)
let push e l = lazy(LList(e,l))

let hd s =
    match Lazy.force s with
    | LList (hd, _) -> hd
    | Empty -> raise LListEmpty

let tl s =
    match Lazy.force s with
    | LList (_, tl) -> tl
    | Empty -> raise LListEmpty

let rec append s1 s2 =
    lazy begin
        match Lazy.force s1 with
        | LList (hd, tl) -> LList (hd, append tl s2)
        | Empty -> Lazy.force s2
    end

let rec map f s =
    lazy begin
        match Lazy.force s with
        | LList (hd, tl) -> LList (f hd, map f tl)
        | Empty -> Empty
    end

let rec iter f s =
    begin
        match Lazy.force s with
        | LList (hd, tl) -> f hd ; iter f tl
        | Empty -> ()
    end

let rec flatten ss =
    lazy begin
        match Lazy.force ss with
        | Empty -> Empty
        | LList (hd, tl) ->
            match Lazy.force hd with
            | LList (hd2, tl2) -> LList (hd2, flatten (lazy (LList (tl2, tl))))
            | Empty -> Lazy.force (flatten tl)
    end

let rec filter_map f s =
    lazy begin
        match Lazy.force s with
        | LList (hd, tl) ->
                begin match f hd with
                |Some y -> LList (y, filter_map f tl)
                |None -> Lazy.force(filter_map f tl)
                end
        | Empty -> Empty
    end

let rec of_list = function
    | [] -> lazy(Empty)
    | hd :: tl -> lazy (LList (hd, of_list tl))

let reverse =
    let rec loop stack = function
        | LList (hd, tl) -> loop (hd :: stack) (Lazy.force tl)
        | Empty -> stack
    in
    fun s ->
        of_list (loop [] (Lazy.force s))

let is_empty s =
    match Lazy.force s with
    |Empty -> true
    |_ -> false

exception Forall
let for_all f e = 
  if is_empty e then true
  else
    try (iter (fun a -> if f a then () else raise Forall) e ; true)
    with Forall -> false

let bind m f = flatten (map f m)
let return a = push a empty

let rec allsets m l =
  if m = 0 then return [] else
  match l with
    [] -> empty
  | h::t -> append (map (fun g -> h::g) (allsets (m - 1) t)) (allsets m t)
;;

let rec range acc a b =
  if a > b then acc
  else range (push a acc) (a+1) b
;;

let rec permutation = function 
  |[] -> return []
  |h::t ->
      bind (permutation t) (fun t1 ->
        map (fun h1 -> (h1 :: t1)) h
      )

let rec bronKerbosch2 gr r p x =
    let n v = N.set_from_vertex gr v in
    if (S.is_empty p) && (S.is_empty x) then (push r empty)
    else
      let s = S.union p x in
      let u = S.choose s in
      let rec fold acc (p,x,s) =
        if S.is_empty s then acc
        else
          let v = S.choose s in
          let rest = S.remove v s in
          let r' = S.union r (S.singleton v) in
          let p' = S.inter p (n v) in
          let x' = S.inter x (n v) in
          let xx = bronKerbosch2 gr r' p' x' in
          fold (append xx acc) (S.remove v p, S.add v x, rest)
      in
      fold empty (p,x,S.diff p (n u))
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

(** connected components. *)
(* XXX : not the most efficient/elegant way, isn't it ? 
    I should do a visit with marking and remove the hashtbl. *)
let connected_components graph =
  let module Dfs = Graph.Traverse.Dfs(UG) in
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
  let l = Hashtbl.fold (fun v k acc -> (v, ref(List.unique !k))::acc ) h [] in
  List.sort ~cmp:(fun (_,k1) (_,k2) -> (List.length !k1) - (List.length !k2)) l 
;;

let install solver _ ll = 
  let aux l = 
    Depsolver_int.S.reset solver.Depsolver_int.constraints;
    match Depsolver_int.solve solver (Diagnostic_int.Lst l) with
    |Diagnostic_int.Failure _ -> false
    |Diagnostic_int.Success _ -> true
  in
  let s = List.fold_left S.union S.empty ll in
  let l = S.elements s in
  aux l
;;

let cmp l1 l2 = (List.length l2) - (List.length l1)

let amend_solver solver q =
  let lit = Depsolver_int.S.lit_of_var (solver.Depsolver_int.map#vartoint q) true in
  Depsolver_int.S.add_un_rule solver.Depsolver_int.constraints lit []
;;
(*
let check_cc reverse mdf a p cc =
  let cs =
    UG.fold_vertex (fun v s ->
      List.fold_right S.add reverse.(v) s
    ) cc S.empty
  in
  let ds =
    List.fold_right S.add 
    (Depsolver_int.dependency_closure mdf [p]) S.empty
  in
  let s = S.inter ds cs in
  try
    (S.iter (fun v -> if not a.(v) then raise Not_found) s ; true)
  with Not_found -> false
;;
*)

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;
  let pkglist = match posargs with [uri] -> parse uri | _ -> assert false in
  let mdf = Mdf.load_from_list pkglist in
  let maps = mdf.Mdf.maps in
  let index = mdf.Mdf.index in
  let solver = Depsolver_int.init_solver index in
  let reverse_t = Depsolver_int.reverse_dependencies mdf in
  let cg = conflictgraph mdf in
  let cc = connected_components cg in
  Printf.eprintf "conflict graph = vertex : %d , edges : %d\n"
  (UG.nb_vertex cg) (UG.nb_edges cg);
  Printf.eprintf "connected components = n. %d , largest : %d\n"
  (List.length cc) (List.length (List.hd (List.sort ~cmp:cmp cc)));
  let ct = conflict_table cc in
  let pt = package_table reverse_t cg ct in
  let a = Array.make (Array.length index) false in
  let hard = ref [] in
  let c = ref 0 in
  List.iter (fun (p,ll) ->
    incr c;
    let size = List.length !ll in
    Printf.printf "(%d or %d) package %s (%d) %!\n" !c (List.length pt) 
    (CudfAdd.print_package (maps.CudfAdd.map#inttovar p)) size ;
    let r =
      for_all (fun m ->
        for_all (fun l ->
          Common.Util.print_info "All sets of size %d (%d)%!\n" m (List.length l); 
          List.iter (fun xl ->
            Common.Util.print_info "-> %s\n" (String.concat " , " (List.map string_of_int !xl))
          ) l ;
          (* l is int list list , gl is int graph list *)
          let gl = List.map (fun xl -> (filter cg !xl)) l in
(*          if List.exists (fun g -> UG.nb_vertex g > 70) gl then
            (hard := (p,ll) :: !hard ; false)
          else *) begin
            let sgl =
              List.sort ~cmp:(fun c1 c2 -> (UG.nb_vertex c1) - (UG.nb_vertex c2)) gl
            in
            (* let sgl = List.filter (check_cc reverse_t a) sgl in *)
            let misl =
              List.map (fun g ->
                let e = 
                  filter_map (fun s ->
                    let sl = (S.elements s) in
                    Common.Util.print_info "%s\n" (String.concat " , " (List.map string_of_int sl)); 
                    if install solver a [s] then Some s else None
                  ) (max_independent_sets g)
                in if is_empty e then assert false else e
              ) sgl
            in
            for_all (fun e -> 
              install solver a ((S.singleton p)::e)
            ) (permutation misl)
          end
        ) (allsets m !ll)
      ) (reverse (range empty 1 size))
    in
    a.(p) <- r ;
    (if r then begin
      amend_solver solver p;
      Printf.printf "%s is always installable\n%!" (CudfAdd.print_package (maps.CudfAdd.map#inttovar p))
    end
    else Printf.printf "eliminates at least one mis\n%!")
  ) pt 
  ;
  let elim = Array.fold_left (fun acc v -> if v then acc + 1 else acc) 0 a in
  let all = let l = ref S.empty in Array.iteri (fun i v -> if v then l := S.add i !l) a ; !l  in
  (if install solver a [all] then print_endline "Success" else print_endline "error" );
  Printf.printf "Total : %d , hard : %d , elim : %d , notelim : %d\n" 
  (List.length pt) (List.length !hard) elim ((List.length pt) - (List.length !hard) - elim)
 ;;

main () ;;
