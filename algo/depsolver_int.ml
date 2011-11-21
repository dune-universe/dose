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

(** Dependency solver. Low Level API *)

(** Implementation of the EDOS algorithms (and more). This module respect the cudf semantic. *)

open ExtLib
open Common

(** progress bar *)
let progressbar_init = Util.Progress.create "Depsolver_int.init_solver"
let progressbar_univcheck = Util.Progress.create "Depsolver_int.univcheck"

(** Message printers *)
let debug fmt = Util.make_debug __FILE__ fmt
let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt

module R = struct type reason = Diagnostic_int.reason end
module S = EdosSolver.M(R)

(** low level solver data type *)
type solver = S.state (** the sat problem *)

(** low level constraint solver initialization
 
    @param buffer debug buffer to print out debug messages
    @param closure init the solver with a subset of packages. This must be
                   the **dependency closure** of the subset of packages.
    @param index package index
 *)
let init_solver ?(buffer=false) univ =
  let num_conflicts = ref 0 in
  let num_disjunctions = ref 0 in
  let num_dependencies = ref 0 in

  let add_depend constraints vpkgs pkg_id l =
    let lit = S.lit_of_var pkg_id false in 
    if (List.length l) = 0 then 
      S.add_rule constraints [|lit|] [Diagnostic_int.Missing(pkg_id,vpkgs)]
    else begin
      let lits = List.map (fun id -> S.lit_of_var id true) l in
      num_disjunctions := !num_disjunctions + (List.length lits);
      S.add_rule constraints (Array.of_list (lit :: lits))
        [Diagnostic_int.Dependency (pkg_id, vpkgs, l)];
      if (List.length lits) > 1 then
        S.associate_vars constraints (S.lit_of_var pkg_id true) l;
    end
  in

  let exec_depends constraints pkg_id pkg =
    List.iter (fun vpkgs ->
      incr num_dependencies;
      add_depend constraints vpkgs pkg_id
      (CudfAdd.resolve_deps_int univ vpkgs)
    ) pkg.Cudf.depends
  in 

  let conflicts = Hashtbl.create 1023 in
  let add_conflict constraints vpkg (i,j) =
    if i <> j then begin
      let pair = (min i j,max i j) in
      (* we get rid of simmetric conflicts *)
      if not(Hashtbl.mem conflicts pair) then begin
        incr num_conflicts;
        Hashtbl.add conflicts pair ();
        let p = S.lit_of_var i false in
        let q = S.lit_of_var j false in
        S.add_rule constraints [|p;q|] [Diagnostic_int.Conflict(i,j,vpkg)];
      end
    end
  in

  let exec_conflicts constraints pkg_id pkg =
    List.iter (fun vpkg ->
      List.iter (fun id ->
        add_conflict constraints vpkg (pkg_id, id)
      ) (CudfAdd.resolve_package_dep univ vpkg)
    ) pkg.Cudf.conflicts
    (*
    List.iter (fun id -> 
      add_conflict constraints (pkg_id, id)
    ) (CudfAdd.resolve_deps_int univ pkg.Cudf.conflicts)
*)
  in

  let size = Cudf.universe_size univ in
  Util.Progress.set_total progressbar_init size ;
  let constraints = S.initialize_problem ~buffer size in

  Cudf.iteri_packages (fun i p ->
    Util.Progress.progress progressbar_init;
    exec_depends constraints i p;
    exec_conflicts constraints i p;
  ) univ;
  Hashtbl.clear conflicts;

  debug "n. disjunctions %d" !num_disjunctions;
  debug "n. dependencies %d" !num_dependencies;
  debug "n. conflicts %d" !num_conflicts;

  S.propagate constraints ;

  constraints
;;

(** return a copy of the state of the solver *)
let copy_solver solver = S.copy solver

(** low level call to the sat solver *)
let solve solver request =
  (* XXX this function gets called a zillion times ! *)
  S.reset solver;

  let result solve collect var =
    if solve solver var then begin
      let get_assignent ?(all=false) () =
        let l = ref [] in
        let a = S.assignment solver in
        for i = 0 to (Array.length a) - 1 do
          if a.(i) = S.True then l := i :: !l
        done;
        !l
      in
      Diagnostic_int.Success(get_assignent)
    end
    else
      let get_reasons () = collect solver var in
      Diagnostic_int.Failure(get_reasons)
  in

  match request with
  |Diagnostic_int.Sng i -> result S.solve S.collect_reasons i
  |Diagnostic_int.Lst il -> result S.solve_lst S.collect_reasons_lst il
;;

let pkgcheck callback solver failed tested id =
  let memo (tested,failed) res = 
    begin
      match res with
      |Diagnostic_int.Success(f_int) ->
          List.iter (fun i -> tested.(i) <- true) (f_int ())
      |Diagnostic_int.Failure _  -> incr failed
    end ; res
  in
  (* try *)
    let req = Diagnostic_int.Sng id in
    let res =
      Util.Progress.progress progressbar_univcheck;
      if not(tested.(id)) then begin
        memo (tested,failed) (solve solver req)
      end
      else begin
        (* this branch is true only if the package was previously
         * added to the tested packages and therefore it is installable *)
        (* if all = true then the solver is called again to provide the list
         * of installed packages despite the fact the the package was already
         * tested. This is done to provide one installation set for each package
         * in the universe *)
        let f ?(all=false) () =
          if all then begin
            match solve solver req with
            |Diagnostic_int.Success(f_int) -> f_int ()
            |Diagnostic_int.Failure _ -> assert false (* impossible *)
          end else []
        in Diagnostic_int.Success(f) 
      end
    in
    match callback with
    |None -> ()
    |Some f -> f (res,req)
  (* with Not_found -> assert false *)
;;

(** [univcheck ?callback (mdf,solver)] check if all packages known by 
    the solver are installable. XXX

    @param mdf package index 
    @param solver dependency solver
    @return the number of packages that cannot be installed
*)
let univcheck ?callback univ =
  let timer = Util.Timer.create "Algo.Depsolver.univcheck" in
  Util.Timer.start timer;
  let solver = init_solver univ in
  let failed = ref 0 in
  let size = Cudf.universe_size univ in
  let tested = Array.make size false in
  Util.Progress.set_total progressbar_univcheck size ;
  let check = pkgcheck callback solver failed tested in
  for i = 0 to size - 1 do check i done;
  Util.Timer.stop timer !failed
;;

(** [listcheck ?callback idlist mdf] check if a subset of packages 
    known by the solver [idlist] are installable

    @param idlist list of packages id to be checked
    @param maps package index
    @return the number of packages that cannot be installed
*)
let listcheck ?callback univ idlist =
  let solver = init_solver univ in
  let timer = Util.Timer.create "Algo.Depsolver.listcheck" in
  Util.Timer.start timer;
  let failed = ref 0 in
  let size = Cudf.universe_size univ in
  Util.Progress.set_total progressbar_univcheck size ;
  let tested = Array.make size false in
  let check = pkgcheck callback solver failed tested in
  List.iter check idlist ;
  Util.Timer.stop timer !failed
;;

(***********************************************************)

(** [reverse_dependencies index] return an array that associates to a package id
    [i] the list of all packages ids that have a dependency on [i].

    @param mdf the package universe
*)

let reverse_dependencies univ =
  let size = Cudf.universe_size univ in
  let reverse = Array.create size [] in
  Cudf.iteri_packages (fun i p ->
    List.iter (fun ll ->
      List.iter (fun q ->
        let j = CudfAdd.vartoint univ q in
        if i <> j then
          if not(List.mem i reverse.(j)) then
            reverse.(j) <- i::reverse.(j)
      ) ll
    ) (CudfAdd.who_depends univ p)
  ) univ;
  reverse

(** [dependency_closure index l] return the union of the dependency closure of
    all packages in [l] .

    @param maxdepth the maximum cone depth (infinite by default)
    @param conjunctive consider only conjunctive dependencies (false by default)
    @param index the package universe
    @param l a subset of [index]

    This function has in a memoization strategy.
*)

let dependency_closure ?(maxdepth=max_int) ?(conjunctive=false) univ =
  let size = Cudf.universe_size univ in
  let h = Hashtbl.create size in
  fun idlist ->
    try Hashtbl.find h (idlist,conjunctive,maxdepth)
    with Not_found -> begin
      let queue = Queue.create () in
      let visited = Hashtbl.create (2 * (List.length idlist)) in
      List.iter (fun e -> Queue.add (e,0) queue) (CudfAdd.normalize_set idlist);
      while (Queue.length queue > 0) do
        let (id,level) = Queue.take queue in
        if not(Hashtbl.mem visited id) && level < maxdepth then begin
          Hashtbl.add visited id ();
          List.iter (function
            |[p] when conjunctive = true ->
              let i = CudfAdd.vartoint univ p in 
              if not(Hashtbl.mem visited i) then
                Queue.add (i,level+1) queue
            |dsj when conjunctive = false ->
              List.iter (fun p ->
                let i = CudfAdd.vartoint univ p in 
                if not(Hashtbl.mem visited i) then
                  if not(Hashtbl.mem visited i) then
                    Queue.add (i,level+1) queue
              ) dsj
            |_ -> ()
          ) (CudfAdd.who_depends univ (CudfAdd.inttovar univ id))
        end
      done;
      let result = Hashtbl.fold (fun k _ l -> k::l) visited [] in
      Hashtbl.add h (idlist,conjunctive,maxdepth) result;
      result
    end

(*    XXX : elements in idlist should be included only if because
 *    of circular dependencies *)
(** return the dependency closure of the reverse dependency graph.
    The visit is bfs.    

    @param maxdepth the maximum cone depth (infinite by default)
    @param index the package universe
    @param idlist a subset of [index]

    This function has in a memoization strategy.
*)
let reverse_dependency_closure ?(maxdepth=max_int) reverse =
  let h = Hashtbl.create (Array.length reverse) in
  let cmp : int -> int -> bool = (=) in
  fun idlist ->
    try Hashtbl.find h (idlist,maxdepth)
    with Not_found -> begin
      let queue = Queue.create () in
      let visited = Hashtbl.create (List.length idlist) in
      List.iter (fun e -> Queue.add (e,0) queue) (List.unique ~cmp idlist);
      while (Queue.length queue > 0) do
        let (id,level) = Queue.take queue in
        if not(Hashtbl.mem visited id) && level < maxdepth then begin
          Hashtbl.add visited id ();
          List.iter (fun i ->
            if not(Hashtbl.mem visited i) then
              Queue.add (i,level+1) queue
          ) reverse.(id)
        end
      done;
      let result = Hashtbl.fold (fun k _ l -> k::l) visited [] in
      Hashtbl.add h (idlist,maxdepth) result;
      result
    end

