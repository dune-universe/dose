(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  Contributor(s):  ADD minor contributors here                          *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common
open Debian
open Algo

let info fmt = Util.make_info __FILE__ fmt
let warning fmt = Util.make_warning __FILE__ fmt
let debug fmt = Util.make_debug __FILE__ fmt
let fatal fmt = Util.make_fatal __FILE__ fmt

let progressbar_u = Util.Progress.create ~unbounded:true "fasU"
let progressbar_b = Util.Progress.create "fasB"

module Boilerplate = BoilerplateNoRpm
module Src = Sources
module Pkg = Packages

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"Detect circular build dependencies"
  include Boilerplate.MakeOptions(struct let options = options end)

  let build_arch = StdOpt.str_option ()
  let target_arch = StdOpt.str_option ()
  let native_arch = StdOpt.str_option ()
  let foreign_archs = Boilerplate.str_list_option ()
  
  let base_system = Boilerplate.vpkglist_option ()

  (*
  let checkonly = Boilerplate.vpkglist_option ()
  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;
  *)

  let dump = StdOpt.str_option ()

  open OptParser

  add options ~long_name:"deb-build-arch" ~help:"Build Architecture" build_arch;
  add options ~long_name:"deb-host-arch" ~help:"Target Architecture" target_arch;
  (* same as host in this context ? *)
  add options ~long_name:"deb-native-arch" ~help:"Native Architecture" native_arch;
  add options ~long_name:"deb-foreign-archs" ~help:"Foregin Architectures" foreign_archs;
  add options ~long_name:"base-system" ~help:"Cross compiled components" base_system;

  add options ~long_name:"dump" ~help:"dump the cudf file" dump;
end

let main () =
  let iteration = ref 0 in

  let posargs = OptParse.OptParser.parse_argv Options.options in
  
  (* enable info / warning / debug information *)
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  
  (* enable a selection of progress bars *)
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["fasB";"fasU"] ;

  (* enable a selection of timers *)
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];

  if not(OptParse.Opt.is_set Options.native_arch) then
    fatal "--deb-native-arch must be specified";

  if not(OptParse.Opt.is_set Options.build_arch) then begin
    info "assume build arch the same as native arch";
    OptParse.Opt.set Options.build_arch (OptParse.Opt.get Options.native_arch);
  end;

  if not(OptParse.Opt.is_set Options.target_arch) then begin
    info "assume target arch the same as native arch";
    OptParse.Opt.set Options.target_arch (OptParse.Opt.get Options.native_arch);
  end;

  let foreign_archs =
    if OptParse.Opt.is_set Options.foreign_archs then 
      OptParse.Opt.get Options.foreign_archs
    else []
  in

  let sourcearchs =
    let native = OptParse.Opt.get Options.native_arch in
    let build = OptParse.Opt.get Options.build_arch in
    let ul = build :: native :: foreign_archs in
    let l = List.map (fun s -> "any-"^s) ul in
    "any" :: "all" :: "linux-any" :: (l @ ul)
  in

  info "Source Archs %s" (String.join "," sourcearchs);

  let binlist, srclist =
    match posargs with
    |[] | [_] -> fatal
      "You must provide a list of Debian Packages files and \
       a Debian Sources file"
    |l ->
        begin match List.rev l with
        |h::t ->
          let archs = sourcearchs in
          let l = Src.input_raw [h] in
          let srcl = Src.sources2packages archs l in
          let pkgl = Pkg.input_raw t in
          (pkgl,srcl)
        |_ -> failwith "Impossible"
        end
  in

  let tables = Debcudf.init_tables (srclist @ binlist) in
  let sl = List.map (fun pkg -> Debcudf.tocudf tables pkg) srclist in
  let bl =
    List.map (fun pkg ->
      Debcudf.tocudf tables (
        (* if List.mem pkg.Pkg.name base_system then 
          {pkg with Pkg.depends = [] ; Pkg.conflicts = []}
        else *) pkg
      )
    ) binlist in
  let pkglist = sl@bl in

  let to_cudf = Debcudf.get_cudf_version tables in

  (*
  let from_cudf = Debcudf.get_real_version tables in
  let pp pkg =
    let p = pkg.Cudf.package in
    let v = from_cudf (CudfAdd.decode pkg.Cudf.package,pkg.Cudf.version) in
    let l =
      List.filter_map (fun k ->
        try Some(k,Cudf.lookup_package_property pkg k)
        with Not_found -> None
      ) ["architecture";"source";"sourcenumber"]
    in (p,v,l)
  in
  *)

  let universe = Cudf.load_universe pkglist in
  let solver = Depsolver_int.init_solver_univ universe in

  let universe_size = Cudf.universe_size universe in
  info "Total packages (source + binaries) %d" universe_size;

  let base_system =
    if OptParse.Opt.is_set Options.base_system then begin
      List.flatten (
        List.map (function
          |(p,None) -> Cudf.lookup_packages universe p
          |(p,Some(c,v)) ->
              let filter = Some(c,to_cudf (p,v)) in
              Cudf.lookup_packages ~filter universe p
        ) (OptParse.Opt.get Options.base_system)
      )
    end else []
  in


  (*
  let checklist =
    if OptParse.Opt.is_set Options.checkonly then begin
      info "--checkonly specified, consider all packages as background packages";
      List.flatten (
        List.map (function
          |(p,None) -> Cudf.lookup_packages universe p
          |(p,Some(c,v)) ->
              let filter = Some(c,snd(to_cudf (p,v))) in
              Cudf.lookup_packages ~filter universe p
        ) (OptParse.Opt.get Options.checkonly)
      )
    end else []
  in
  *)


  if OptParse.Opt.is_set Options.dump then begin
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file";
    Cudf_printer.pp_preamble oc Debcudf.preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc universe
  end;

  (* build a table that associate to each source :
    - binary list
    - build dependency list
  *)
  let module PkgE = struct
    type t = int ref
    let compare x y = Pervasives.compare !x !y
    let hash x = Hashtbl.hash !x
    let equal x y = !x = !y
    let default = ref (-10000)
  end in

  let module PG = Defaultgraphs.MakePackageGraph(Defaultgraphs.PkgV)(PkgE) in

  let module G = PG.G in
  let module D = PG.D in
  let module C = Graph.Components.Make(G) in
  let module Dfs = Graph.Traverse.Dfs(G) in
  let module O = Defaultgraphs.GraphOper(G) in

  let module SV = Set.Make(G.V) in
  let module SE = Set.Make(
    struct
      type t = G.E.t 
      let compare e1 e2 = 
        if G.E.compare e1 e2 = 0 then 1 else
          if !(G.E.label e1) = !(G.E.label e2) then 1
          else !(G.E.label e1) - !(G.E.label e2)
    end) 
  in

  let to_set l = List.fold_right SV.add l SV.empty in

  let partition s w = snd(SV.partition (fun e -> e >= w) s) in

  let print_set_e s =
    String.join " " (List.map (fun e -> 
      (CudfAdd.string_of_package e)
      ) (SV.elements s))
  in

  let edge_to_string (s,l,d) = 
    Printf.sprintf "%s -(%d)-> %s" 
    (CudfAdd.string_of_package s)
    !l
    (CudfAdd.string_of_package d)
  in

  (* returns a new graph containg a copy of all edges and vertex of g *)
  let copy_graph g =
    let g1 = G.create () in
    G.iter_edges_e (fun e -> G.add_edge_e g1 e) g;
    G.iter_vertex (fun v -> G.add_vertex g1 v) g;
    g1
  in

  (* return subgraph that contains all vertex in s and all edges that connet
   * two vertex in s *)
  let extract_subgraph g s =
    let sg = G.create () in
    SV.iter (fun e -> G.add_vertex sg e) s;
    G.iter_edges_e (fun e ->
      let v1 = G.E.src e in let v2 = G.E.dst e in
      if SV.mem v1 s && SV.mem v2 s then
        G.add_edge_e sg e
    ) g;
    sg
  in

  (* return a new graph without all the edges in l - as G.E.t list *)
  let remove_edges_e g l =
    let sg = copy_graph g in
    List.iter (G.remove_edge_e sg) l;
    sg
  in

  let hash_to_list t = Hashtbl.fold (fun k _ acc -> k::acc) t [] in

  (* return one cycle in g, if one exists *)
  (* the problem here is that this simple cycle is does not have a minimal weigth *)
  let find_simple_cycle g =
    let clean es =
      let l = ref SE.empty in
      let ll = ref [] in
      try
        Stack.iter (fun e ->
          ll:= (G.E.dst e)::!ll;
          if List.mem (G.E.src e) !ll then (l:= SE.add e !l ; raise Exit)
          else l:= SE.add e !l
        ) es; SE.empty
      with Exit -> !l
    in
    let h = Hashtbl.create (G.nb_vertex g) in
    let es = Stack.create () in
    let rec visit v =
      Hashtbl.add h v true;
      G.iter_succ_e (fun e ->
        let w = G.E.dst e in
        Stack.push e es;
        try if Hashtbl.find h w then raise Exit else ignore(Stack.pop es)
        with Not_found -> visit w ;
      ) g v;
      if not (Stack.is_empty es) then ignore(Stack.pop es);
      Hashtbl.replace h v false;
    in
    try G.iter_vertex (fun v -> if not (Hashtbl.mem h v) then visit v) g;
    SE.empty
    with Exit -> clean es
  in

  (* return a feedback arc set of a weight graph *)
  let fas sg =
    let g = copy_graph sg in
    (* f is a set of edges to remove *)
    let f = Hashtbl.create 1023 in
    let weight e = !(G.E.label e) in
    let update_weight e w = let l = G.E.label e in l := w in
    let add k = Hashtbl.replace f k () in
    let subgraph = ref g in
    let is_stuck = ref (Hashtbl.length f) in
    while Dfs.has_cycle !subgraph do
      Util.Progress.progress progressbar_u;
      let c = find_simple_cycle !subgraph in
      (* print_set_e "simple cycle" c; *)

      (* edge with min weight *)
      let eps = weight (SE.min_elt c) in
      Printf.printf "min %d\n%!" eps ;
      SE.iter (fun e ->
        Printf.printf "w e %d\n%!" (weight e) ;
        if (weight e) <= 0 then () else begin
          update_weight e ((weight e) - eps);
          Printf.printf "update %d\n%!" (weight e) ;
          if (weight e) <= 0 then (
            Printf.printf "Candidate to be removed %s \n%!" (edge_to_string e);
            (* we remove only one edge per cycle ... *)
            add e 
          )
        end
      ) c;

      if (Hashtbl.length f) = !is_stuck then raise Exit;
      is_stuck := Hashtbl.length f;
      subgraph := remove_edges_e !subgraph (hash_to_list f);
    done;
    Util.Progress.set_total progressbar_b (Hashtbl.length f);
    Hashtbl.iter (fun e _ ->
      Util.Progress.progress progressbar_b;
      let (v,w) = G.E.src e, G.E.dst e in
      let sl = hash_to_list f in
      let sub = remove_edges_e g sl in

      G.add_edge_e sub e;
      if not(Dfs.has_cycle sub) then begin
        Hashtbl.remove f e;
      end
    ) f;
    hash_to_list f
  in

  let print_dot s g =
    let oc = open_out s in
    D.output_graph oc g;
    close_out oc
  in

  let is_build_essential pkg =
    try bool_of_string (Cudf.lookup_package_property pkg "buildessential") 
    with Not_found -> false
  in
  
  let is_essential pkg =
    try bool_of_string (Cudf.lookup_package_property pkg "essential") 
    with Not_found -> false
  in

  let get_source pkg =
    let sn = try Cudf.lookup_package_property pkg "source" with Not_found -> fatal "WTF source" in
    let sv = try Cudf.lookup_package_property pkg "sourcenumber" with Not_found -> fatal "WTF sourcenumber"in
    try 
      Cudf.lookup_package universe (CudfAdd.encode ("src"^Src.sep^sn),to_cudf (sn,sv))
    with Not_found -> begin
      warning "The source package %s %s associated to the bin package %s is missing" 
      sn sv (CudfAdd.string_of_package pkg);
      raise Not_found
    end
  in

  let add_source g src =
    (* source -> bin (build dep) *)
    (* build essential or essential -> 2000
     * direct dependency and doc -> 10
     * direct dependency and lib -> 1
     * direct dependency -> 100
     * not a direct dependency -> 500
     *)
    let is_direct_build_dep src bin =
      try
        ignore (
          List.find (fun p -> CudfAdd.equal bin p) (List.flatten (CudfAdd.who_depends universe src))
        ); true
      with Not_found -> false
    in
    let is_doc bin = String.ends_with bin.Cudf.package "-doc" in
    let is_lib bin = String.ends_with bin.Cudf.package "-dev" in
    let assign_label src bin =
      let l = 
        if is_build_essential bin || is_essential bin then 2000
        else 
          if is_direct_build_dep src bin then 
            if is_doc bin then 10 else
              if is_lib bin then 1 else 100
          else 500 
      in ref l
    in

    (* if not (G.mem_vertex g src) then *) begin
      let req = Diagnostic_int.Sng (CudfAdd.vartoint universe src) in
      let res = Depsolver_int.solve solver req in
      (* effectively here we select ONE solution to satsfy the build 
       * dependencies of this package. There can be many of these solutions
       * and a backtrack step might be required. This simplification
       * make this algorithm not exaustive... *)
      let closure =
        match res with
        |Diagnostic_int.Success f_int ->
            List.map (CudfAdd.inttovar universe) (f_int ~all:true ())
        |_ -> info "broken source %s " (CudfAdd.string_of_package src) ; []
      in
      let preds = G.pred g src in
      List.iter (fun bin ->
        (* if the source package depends on a package in the base system,
         * we do not add this package *)
        if not (CudfAdd.equal bin src) then begin
          let label = assign_label src bin in
          let e = G.E.create src label bin in
          info "Add Edge %s" (edge_to_string e);
          if not(List.mem bin preds) then G.add_edge_e g e
        end
      ) closure
    end
  in

  (* find an installation set of the build dependencies : the smallest ? *)
  (* get the bin list, add the src package and ask for an
   * installation set of this src in this universe *)
  (* add to the graph all the runtime dependencies that are
   * in this installation set *)
  (* create the src-dependency graph *)
  let g = G.create () in
  List.iter (fun bin ->
    try 
      let src = get_source bin in
      (* bin -> source (belongs to) *)
      (* I never want to break such dependency *)
      let label = ref 100000 in
      let e = G.E.create bin label src in
      G.add_edge_e g e;
    with Not_found -> () (* from get_source *)
  ) bl ;
  List.iter (fun bin ->
    try 
      let src = get_source bin in
      begin try add_source g src with Not_found -> () end;
    with Not_found -> () (* from get_source *)
  ) bl ;

  let remove_source g src =
    info "Remove source %s" (CudfAdd.string_of_package src);
    if G.mem_vertex g src then begin 
      G.iter_pred (fun bin ->
        info "Remove binary %s" (CudfAdd.string_of_package bin);
        G.remove_vertex g bin
      ) g src;
      G.remove_vertex g src
    end
  in

  let reduce g =
    List.iter (fun src ->
      if G.mem_vertex g src then
        if G.out_degree g src = 0 then
          remove_source g src
    ) sl
  in

  let remove_base_packages g l =
    List.iter (fun pkg ->
      let src = 
        if List.mem pkg bl then
          get_source pkg
        else pkg
      in
      remove_source g src
    ) l
  in
 
  (* break dependency loops. This should use a heuristic of some kind or
   * ask for user intervention *)
  let reduce_loops g =
    let scc = C.scc_array g in
    info "SCC # %d" (Array.length scc);
    Array.iteri (fun i l ->
      if List.length l > 1 then begin 
        info "Loop detected (%d)" (List.length l);
        (*
        info "Loop : %s" (
          String.join " -> " (List.map (fun pkg -> 
            CudfAdd.string_of_package pkg
          ) l));
          *)
        let sg = extract_subgraph g (to_set l) in
        let subname = Printf.sprintf "sub-%d-%d.dot" !iteration i in
        print_dot subname sg;

        let feedback_set = fas sg in
        info "arcs to remove %d" (List.length feedback_set);
        List.iter (fun (v1,l,v2) ->
          info "Remove %s" (edge_to_string (v1,l,v2));
          G.remove_edge sg v1 v2
        ) feedback_set;
        info "cycles %b\n" (Dfs.has_cycle sg);
        let subname = Printf.sprintf "sub-%d-%d-fas.dot" !iteration i in
        print_dot subname sg;

      end
    ) scc;
  in

  print_dot "reduce-start.dot" g;
  remove_base_packages g base_system;
  print_dot "reduce-base.dot" g;
  reduce g;
  print_dot "reduce-1.dot" g;
  reduce g;
  print_dot "reduce-2.dot" g;
  reduce_loops g;

  (*
  while G.nb_vertex g > 0 do

    info "Nodes %d %d (this should descrease)" (G.nb_vertex g) (G.nb_edges g);
    info "Reduce bin (#%d)" !iteration;
    reduce_bin g;
    print_dot (Printf.sprintf "reduce-%d-bin.dot" !iteration) g;
    info "Reduce src (#%d)" !iteration;
    reduce_src g;
    print_dot (Printf.sprintf "reduce-%d-src.dot" !iteration) g;
    info "Reduce loops (#%d)" !iteration;
    reduce_loops g;
    print_dot (Printf.sprintf "reduce-%d-loops.dot" !iteration) g;
    incr iteration;

  done
*)
 
 
  (* parse the list of packages to include in the base system and
   * build a list of packages that have the same version as the package
   * in the binlist but no dependencies or conflicts. These packages must
   * always be installable. they are the dummy packages built using a cross
   * compiler . One goal is to find the smallest set needed to bootstrap a
   * system *)

  (* All packages in the base system must always be installable *)

  (* for all sources :
    - have no build dependencies
    - have all build dependencies satisfied in the base system.
      For all binaries of such sources
      - if the binary is installable in the base system, then add it to it.
      - otherwise add the source of its dependencies to the buildqueue
    repeat until no other source can be marked as compiled
   *)

  (* what I'm left with is a bunch of source that cannot be compiled because of
   * circular dependencies *)

  (* break one, start from the beginning *)
  (* which one to break ? *)
  (* how to break it ? *)

;;

main ();;

