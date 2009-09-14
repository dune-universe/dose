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

open Cudf
open ExtLib
open Common
open Algo
open Graph

(* enable the progress bar for strongdeps *)
Common.Util.Progress.enable "Algo.Strongdep.main";;
Common.Util.Progress.enable "Algo.Strongdep.conj";;

exception Done

module Options = struct
  let confile = ref ""
  let dot = ref false
  let info = ref false
  let dump = ref false
  let strong_pred = ref false
  let src = ref ""
  let dst = ref ""
end

let usage = Printf.sprintf "usage: %s [-options] [cudf doc]" (Sys.argv.(0))
let options =
  [
   ("--confile",  Arg.String (fun l -> Options.confile := l ), "Specify a configuration file" );
   ("--dot", Arg.Set Options.dot, "Print the graph in dot format");
   ("--src",  Arg.String (fun l -> Options.src := l ), "Specify a list of packages to analyze" );
   ("--dst",  Arg.String (fun l -> Options.dst := l ), "Specify a pivot package" );
   ("--info", Arg.Set Options.info, "Print various aggregate information");
   ("--dump", Arg.Set Options.dump, "Dump the strong dependency graph in graph.marshal");
   ("--pred", Arg.Set Options.strong_pred, "Print strong predecessor (not direct)");
   ("--debug", Arg.Unit (fun () -> Common.Util.set_verbosity Common.Util.Summary), "Print debug information");
  ]

let and_sep_re = Str.regexp "\\s*;\\s*"
let pkg_re = Str.regexp "(\\([a-z][a-z0-9.+-]+\\)\\s*,\\s*\\([0-9][0-9]*\\))"
let parse_pkg s =
  let parse_aux str =
    if Str.string_match pkg_re str 0 then begin
      (Str.matched_group 1 str, int_of_string (Str.matched_group 2 str))
    end
    else
      (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Str.split and_sep_re s)

(* ----------------------------------- *)

module StrongDep = Strongdeps.Make(Defaultgraphs.BidirectionalGraph.G)

(* ----------------------------------- *)

let main () =
  at_exit (fun () -> Common.Util.dump Format.err_formatter);
  let uri = ref "" in
  let _ =
    try Arg.parse options (fun f -> uri := f ) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  Printf.eprintf "Parsing and normalizing...%!" ;
  let timer = Common.Util.Timer.create "Parsing and normalizing" in
  Common.Util.Timer.start timer;
  let pkglist =
    match Input.parse_uri !uri with
    |(("pgsql"|"sqlite") as dbtype,info,(Some query)) ->
IFDEF HASDB THEN
      begin
        Backend.init_database dbtype info (Idbr.parse_query query) ;
        let l = Backend.load_selection (`All) in
        List.map Debian.Debcudf.tocudf l
      end
ELSE
      failwith (dbtype ^ " Not supported")
END
(*
    |("debsrc",(_,_,_,_,file),_) -> begin
      let l = Debian.Source.input_raw [file] in
      let sl = Debian.Source.sources2packages "" l in
      List.map Debian.Debcudf.tocudf sl
    end
*)
(* XXX here I don't want a semantic translation to cudf !!! *)
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

  let g = StrongDep.strongdeps pkglist in
  Defaultgraphs.BidirectionalGraph.D.output_graph stdout g;
  print_newline ()

;;

main ();;

(* ---------------Old Functions - Not Used -------------------- *)

let direct_dep h p1 p2 =
  try
    match Hashtbl.find h p1 with
    |[],_ -> false
    |ll,_ -> List.exists (List.mem p2) ll
  with Not_found -> assert false
;;

(* Dijkstra shortest path - modified *)
let find_path filter h v1 v2 =
  let visited = Hashtbl.create 97 in
  let heap = ref [] in
  let rec loop () =
    match !heap with
    |[] -> raise Not_found
    |(v,p)::_ when v = v2 -> List.rev p
    |(v,p)::tl ->
        begin
          heap := tl;
          if not (Hashtbl.mem visited v) then begin
            Hashtbl.add visited v ();
            List.iter (fun l ->
              List.iter (fun e ->
                if filter e then
                  heap := ((e, e :: p)::!heap)
              ) l
            ) (fst(Hashtbl.find h v))
          end;
          loop ()
        end
  in
  heap := (v1, [])::!heap;
  loop ()
;;


let rec add_sd_path f p = function
  |hd::tl when p = hd -> add_sd_path f hd tl
  |hd::tl -> f p hd ; add_sd_path f hd tl
  |[] -> ()
;;


let rec addlist f h p = function
  |[] -> ()
  |pid::t as l ->
      List.iter(fun pid -> if not (Hashtbl.mem h pid) then f(p,pid)) l ;
      if not (Hashtbl.mem h pid) then addlist f h pid t
;;


(*

  let print_info_solver problem p1 r instset dc =
    print_debug 5 "Package %s\n" (print_package p1) ;
    print_debug 5 "N. conflicts constraints %i\n" problem.conflicts ;
    print_debug 5 "N. dependencies constraints %i\n" problem.dependencies ;
    print_debug 5 "Problem Size %i\n" problem.size ;
    print_debug 5 "Installation Set Size %i\n" r.D.size ;
    print_debug 4 "Installation set %s : %s\n"
      (print_package p1)
      (String.concat ","
      (List.map (fun p ->
        Printf.sprintf "%s" (print_package p)
        ) instset
      ))
    ;
    print_debug 4 "Dependency Closure %s : %s\n"
      (print_package p1)
      (String.concat ","
      (List.map (fun (p,_,_) ->
        Printf.sprintf "%s" (print_package p)
        ) dc
      ))
    ;
    flush_all ()
  in

  let print_info (tot,o,i1,i2,sk) =
    Printf.eprintf "Analyzed %i packages out of %i\n" o tot ;
    Printf.eprintf "Skipped Packages %i\n" sk ;
    Printf.eprintf "Strong Dep Count %d\n" (G.nb_edges graph);
    Printf.eprintf "Strong Dep Graph Vertex %d\n" (G.nb_vertex graph);
    Printf.eprintf "N. of Calls to edos_install %i\n" i1 ;
    Printf.eprintf "N. of Calls to check_depends %i\n\n" i2 ;
    flush_all ();
  in

  let print_stats tot =
    let step = 20000 in
    let i = ref 0 in
    function (o,i1,i2,sk) ->
      if !i > step && (!Options.debug = 1) then begin
        print_info (tot,o,i1,i2,sk);
        i := 0
      end
      else incr i
  in

  let size = (List.length available) in
  let print_stats = print_stats size in
  let outer = ref 0 in
  let inner1 = ref 0 in
  let inner2 = ref 0 in
  let skipcount = ref 0 in

  let pkg_src () =
    let l = ref [] in
    List.iter (fun (p,v) ->
      let pid = get_pid(p,v) in
      let (dl,cl) = Hashtbl.find availableHash pid in
      l := (pid,dl,cl)::!l
    ) (parse_pkg !Options.src)
    ;
    !l
  in
  let pkg_dst () =
    (* all packages q in R s.t. q is in the dependency closure of p *)
    let (p,v) = List.hd(parse_pkg !Options.dst) in
    let pid = get_pid(p,v) in
    List.filter_map (fun ((p,_,_) as pkg) ->
      if List.mem pid (memo_dc p) then
        Some(pkg)
      else None
    ) available
  in

  let pkg_src_list = ref [] in
  let pkg_dst_list = ref [] in
  let plist = 
    if !Options.src <> "" && !Options.dst <> "" then begin
      let (p,v) = List.hd(parse_pkg !Options.dst) in
      let pid = get_pid(p,v) in
      let (dl,cl) = Hashtbl.find availableHash pid in
      let dst = (pid,dl,cl) in
      pkg_src_list := pkg_src ();
      pkg_dst_list := [dst];
      (dst::!pkg_src_list)
    end
    else if !Options.src <> "" then begin
      pkg_src_list := pkg_src ();
      !pkg_src_list
    end
    else if !Options.dst <> "" then begin
      pkg_dst_list := pkg_dst ();
      !pkg_dst_list
    end
    else available
  in
  strongdeps (sort plist)
  ;

  if !Options.dump then begin
    let out_ch = open_out "graph.marshal" in
    Marshal.to_channel out_ch graph [] ;
    close_out out_ch
  end
  ;

  if !Options.info then begin
    print_info (size,!outer,!inner1,!inner2,!skipcount) ;
    if !Options.dst <> "" then begin
      let (p,v) = List.hd(parse_pkg !Options.dst) in
      let pid = get_pid(p,v) in
      let in_d = G.in_degree graph pid in
      Printf.printf "In degree for (%s,%d) : %d \n" p v in_d ;
    end
  end
  ;

  let module Display = 
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (print_package v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v =
        if List.exists (fun (p,_,_) -> p = v) !pkg_src_list then
          [`Color 1]
        else if List.exists (fun (p,_,_) -> p = v) !pkg_dst_list then
          [`Color 10]
        else []

     let edge_attributes e =
        let t = 
          match G.E.label e with
          |PkgE.Strong -> [`Style `Bold]
          |PkgE.Direct -> [`Style `Dotted]
          |PkgE.Disjunctive -> [`Style `Dashed]
        in
        if (List.exists (fun (p,_,_) -> p = (G.E.src e)) !pkg_src_list) &&
           (List.exists (fun (p,_,_) -> p = (G.E.dst e)) !pkg_dst_list) then
          (`Color 5) :: t
        else t
    end
  in
  let module D = Graph.Graphviz.Dot(Display) in
  let module T = Graph.Traverse.Dfs(G) in
  let module I = Oper.I(G) in
  let sd_graph graph = 
    (* we create a new graph ignoring uninteresting edges/vertex, 
     * that is direct dependencies and vertex without in/out strong
     * (real) dependencies *)
    let gr = G.create () in
    G.iter_vertex (fun v ->
      let f v =
        let out_strong =
          List.exists (fun e ->
            (G.E.label e) = PkgE.Strong
          ) (G.succ_e graph v)
        in
        let in_strong =
          List.exists (fun e ->
            (G.E.label e ) = PkgE.Strong
          ) (G.succ_e graph v)
        in
        if (in_strong || out_strong) then begin
          G.add_vertex gr v;
          G.iter_succ_e (fun e ->
            G.add_edge_e gr e
          ) graph v
        end
      in
      T.prefix_component f graph v
    ) graph
    ;
    gr
  in

  if !Options.dot then begin
    let dg = dependency_graph plist in
    D.output_graph stdout (sd_graph (I.union dg graph)) 
  end
  else if (!Options.dst <> "") && !Options.strong_pred && not !Options.dot then
    let (p,v) = List.hd(parse_pkg !Options.dst) in
    let pid = get_pid(p,v) in
    strong_pred print_package graph pid
  else if not (!Options.info) then begin 
    sensitivity print_package availableHash dependency_closure_exp graph;
(*    print_newline ();
    let dg = dependency_graph plist in (* XXX plist where !! *)
    print_dom print_package dg ;
    *)
  end

*)
