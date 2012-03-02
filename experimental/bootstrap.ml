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

let progressbar = Util.Progress.create "fas"

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
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) ["fas"] ;

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
    Cudf_printer.pp_universe oc universe
  end;

  (* build a table that associate to each source :
    - binary list
    - build dependency list
  *)

  let module PG = Defaultgraphs.PackageGraph in
  let module G = PG.G in
  let module D = PG.D in
  let module C = Graph.Components.Make(G) in

  let copy_graph g =
    let g1 = G.create () in
    G.iter_edges_e (fun e -> G.add_edge_e g1 e) g;
    G.iter_vertex (fun v -> G.add_vertex g1 v) g;
    g1
  in

  let fas sg =
    info "subgraph (%d)" (G.nb_vertex sg);
    let to_list t = Hashtbl.fold (fun k _ acc -> k::acc) t [] in
    Util.Progress.set_total progressbar (G.nb_vertex sg) ;
    let g = copy_graph sg in
    let f = Hashtbl.create 1023 in
    let vertex_set = Stack.create () in
    G.iter_vertex (fun v -> Stack.push v vertex_set) g ;
    info "before loop";
    while G.nb_vertex g > 0 do
      Util.Progress.progress progressbar;
      let v = Stack.pop vertex_set in
      if G.in_degree g v < G.out_degree g v then
        G.iter_pred (fun v1 -> Hashtbl.add f (v1,v) ()) g v
      else
        G.iter_succ (fun v2 -> Hashtbl.add f (v,v2) ()) g v
      ;
      (* this removes all incoming and outgoing arcs to v *)
      G.remove_vertex g v
    done;
    Util.Progress.reset progressbar;
    to_list f
  in

  let print_dot s g =
    let oc = open_out s in
    D.output_graph oc g;
    close_out oc
  in

  let add_source g src =
    if not (G.mem_vertex g src) then begin
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
      List.iter (fun bin ->
        (* if the source package depends on a package in the base system,
         * we do not add this package *)
        if not (CudfAdd.equal bin src) then
          G.add_edge g src bin
          (* source -> bin (build dep) *)
      ) closure
    end
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

  (* find an installation set of the build dependencies : the smallest ? *)
  (* get the bin list, add the src package and ask for an
   * installation set of this src in this universe *)
  (* add to the graph all the runtime dependencies that are
   * in this installation set *)
  let g = G.create () in
  List.iter (fun bin ->
    try 
      let src = get_source bin in
      begin try add_source g src with Not_found -> () end;
      (* bin -> source (belongs to) *)
      G.add_edge g bin src;

    with Not_found -> () (* from get_source *)
  ) bl ;
  
  let remove_source g src =
    G.iter_pred (fun bin ->
      (* info "Remove binary %s" (CudfAdd.string_of_package bin); *)
      G.remove_vertex g bin
    ) g src;
    info "Remove source %s" (CudfAdd.string_of_package src);
    G.remove_vertex g src
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
 
  (*
  let f g =
    let module VS = 
      Set.Make (struct
        type t = G.V.t
        let compare p q = compare (G.in_degree g q) (G.in_degree g p)
      end) 
    in

    let s = List.fold_right VS.add bl VS.empty in
    VS.iter (fun p ->
      Printf.printf "%s (%d)\n" (CudfAdd.string_of_package p) (G.in_degree g p);
    ) s
  in
  *)

  let subgraph g l =
    let sg = G.create () in
    List.iter (fun src_up ->
      if String.starts_with src_up.Cudf.package "src%3a" then
        G.iter_succ (fun pkg -> 
          if String.starts_with pkg.Cudf.package "src%3a" then
            fatal "%s -> %s" (CudfAdd.string_of_package src_up) (CudfAdd.string_of_package pkg);
          let sn = try Cudf.lookup_package_property pkg "source" with Not_found -> fatal "WTF source" in
          let sv = try Cudf.lookup_package_property pkg "sourcenumber" with Not_found -> fatal "WTF sourcenumber"in
          try
            let src_down = Cudf.lookup_package universe (CudfAdd.encode ("src"^Src.sep^sn),to_cudf (sn,sv)) in
            if not (CudfAdd.equal src_up src_down) then
              G.add_edge sg src_up src_down
          with Not_found -> info "missing %s %s ?" sv sv
        ) g src_up
    ) l;
    sg
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
        let sg = subgraph g l in
        info "subgraph";
        let subname = Printf.sprintf "sub-%d-%d.dot" !iteration i in
        print_dot subname sg;

        let feedback_set = fas sg in
        info "arcs to remove %d" (List.length feedback_set);
        List.iter (fun (v1,v2) ->
          info "Remove %s -> %s" 
          (CudfAdd.string_of_package v1)
          (CudfAdd.string_of_package v2);
          G.remove_edge sg v1 v2
        ) feedback_set;
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

