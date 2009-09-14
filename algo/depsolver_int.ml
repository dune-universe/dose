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

(** Implementation of the EDOS algorithms *)
(* this module respect the cudf semantic. *)

open ExtLib
open Common

module R = struct type reason = Diagnostic_int.reason end
module S = EdosSolver.M(R)

type solver = {
  constraints : S.state ;
  size : int ;
  conflicts : int ;
  disjunctions : int ;
  dependencies : int
}

let progressbar = Util.Progress.create "Depsolver.init_solver"

(* low level constraint solver initialization
 * @param: buffer : debug buffer to print out debug messages
 * @param: proxy_size : proxy variables. These are additional variables 
 *                      used to encode specific contraint.
 * @param: idlist : init the solver with a subset of packages id
 * @param: index : 
 *)
let init_solver ?(buffer=false) ?(proxy_size=0) ?idlist index =
  let size = (Array.length index) + proxy_size in
  let constraints = S.initialize_problem ~buffer:buffer size in
  let num_conflicts = ref 0 in
  let num_disjunctions = ref 0 in
  let num_dependencies = ref 0 in

  (* add dependencies *)
  let exec_depends pkg_id pkg =
    let lit = S.lit_of_var pkg_id false in
    for i = 0 to (Array.length pkg.Mdf.depends) - 1 do
      incr num_dependencies;
      let (vpkg,disjunction,dependencies) = pkg.Mdf.depends.(i) in
      if Array.length disjunction = 0 then
        S.add_un_rule constraints lit [Diagnostic_int.EmptyDependency(pkg_id,vpkg)]
      else begin
        let lit_array =
          let a =
            Array.map (fun i -> 
              incr num_disjunctions;
              S.lit_of_var i true
            ) disjunction 
          in
          Array.append [|lit|] a
        in
        S.add_rule constraints lit_array
        [Diagnostic_int.Dependency(pkg_id,Array.to_list disjunction)]
        ;
        if Array.length disjunction > 1 then
          S.associate_vars constraints
          (S.lit_of_var pkg_id true)
          (Array.to_list disjunction)
      end
    done
  in

  (* add conflicts *)
  let exec_conflicts pkg_id1 pkg =
    let conjunction = pkg.Mdf.conflicts in
    let x = S.lit_of_var pkg_id1 false in 
    for i = 0 to (Array.length conjunction) - 1 do
      let (pkg1, pkg_id2) = conjunction.(i) in
      if pkg_id1 <> pkg_id2 then begin
        incr num_conflicts;
        let y = S.lit_of_var pkg_id2 false in
        S.add_bin_rule constraints x y [Diagnostic_int.Conflict(pkg_id1, pkg_id2)]
      end
    done
  in

  begin match idlist with
  |None -> 
      for i = 0 to (Array.length index) - 1 do
        exec_depends i index.(i);
        exec_conflicts i index.(i); 
      done
  |Some(l) ->
      List.iter (fun i ->
        exec_depends i index.(i);
        exec_conflicts i index.(i);
      ) l
  end
  ;

  S.propagate constraints ;

  {
    constraints = constraints ;
    size = Array.length index ;
    conflicts = !num_conflicts ;
    dependencies = !num_dependencies ;
    disjunctions = !num_disjunctions
  }
;;

let copy_solver solver =
  { solver with constraints = S.copy solver.constraints }

let solve solver request =
  S.reset solver.constraints;

  let result solve collect ?(ignore= -1) var =
    if solve solver.constraints var then begin
      let get_assignent () = 
        let l = ref [] in
        Array.iteri(fun i a ->
          if (a = S.True) && (i <> ignore) then l := i::!l 
        ) (S.assignment solver.constraints)
        ;
        !l
      in 
      Diagnostic_int.Success(get_assignent)
    end
    else
      let get_reasons () = collect solver.constraints var in
      Diagnostic_int.Failure(get_reasons)
  in

  match request with
  |Diagnostic_int.Req i -> result S.solve S.collect_reasons ~ignore:i i
  |Diagnostic_int.Sng i -> result S.solve S.collect_reasons i
  |Diagnostic_int.Lst il -> result S.solve_lst S.collect_reasons_lst il
;;

(***********************************************************)

let dependency_closure index l =
  let queue = Queue.create () in
  let visited = Hashtbl.create 1024 in
  List.iter (fun e -> Queue.add e queue) (List.unique l);
  while (Queue.length queue > 0) do
    let id = Queue.take queue in
    if not(Hashtbl.mem visited id) then begin
      Hashtbl.add visited id ();
      Array.iter (fun (_,dsj,_) ->
        Array.iter (fun i ->
          if not(Hashtbl.mem visited i) then
            Queue.add i queue
        ) dsj
      ) index.(id).Mdf.depends
    end
  done ;
  Hashtbl.fold (fun k _ l -> k::l) visited []

let pkgcheck callback solver failed tested id =
  try
    if not(tested.(id)) then begin
      let req = Diagnostic_int.Sng id in
      let res = solve solver req in
      begin match res with
      |Diagnostic_int.Success(f) -> 
          begin try List.iter (fun i -> tested.(i) <- true) (f ())
          with Not_found -> assert false end
      |_ -> incr failed
      end
      ;
      match callback with
      |None -> ()
      |Some f -> f (res,req)
    end
  with Not_found -> assert false

let listcheck ?callback idlist mdf =
  let solver = init_solver ~idlist mdf.Mdf.index in
  let timer = Util.Timer.create "Algo.Depsolver.listcheck" in
  Util.Timer.start timer;
  let failed = ref 0 in
  let tested = Array.make (Array.length mdf.Mdf.index) false in
  let check = pkgcheck callback solver failed tested in
  List.iter check idlist ;
  Util.Timer.stop timer !failed
;;

let univcheck ?callback (mdf,solver) =
  let timer = Util.Timer.create "Algo.Depsolver.univcheck" in
  Util.Timer.start timer;
  let failed = ref 0 in
  let tested = Array.make (Array.length mdf.Mdf.index) false in
  let check = pkgcheck callback solver failed tested in
  for i = 0 to (Array.length mdf.Mdf.index) - 1 do check i done;
  Util.Timer.stop timer !failed
;;

(************************************************)

(*
let __conflict_closure maps l =
  List.fold_left (fun acc pkg ->
    let l = maps.who_conflicts pkg in
    List.fold_left (fun set p -> PKGS.add p set) acc l
  ) PKGS.empty l

let conflict_closure maps l =
  try
    let s = __conflict_closure maps l in
    PKGS.elements s
  with _ -> failwith "conflict closure"

let cone maps l =
  try
    let s = __dependency_closure maps l in
    let c = 
      PKGS.fold (fun pkg acc ->
        let l = maps.who_conflicts pkg in
        List.fold_left (fun set p -> PKGS.add p set) acc l
      ) s PKGS.empty
    in
    PKGS.elements (PKGS.union c s)
  with _ -> failwith "cone closure"

(***********************************************************)


*)
