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
  
  let base_system = Boilerplate.str_list_option ()

  (* let checkonly = Boilerplate.vpkglist_option ()
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

  let posargs = OptParse.OptParser.parse_argv Options.options in
  
  (* enable info / warning / debug information *)
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  
  (* enable a selection of progress bars *)
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;

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

  let base_system =
    if OptParse.Opt.is_set Options.base_system then 
      OptParse.Opt.get Options.base_system
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
        if List.mem pkg.Pkg.name base_system then 
          {pkg with Pkg.depends = [] ; Pkg.conflicts = []}
        else pkg
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

  if OptParse.Opt.is_set Options.dump then begin
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file";
    Cudf_printer.pp_universe oc universe
  end;

  (* build a table that associate to each source :
    - binary list
    - build dependency list
  *)

  let module G = Defaultgraphs.PackageGraph.G in
  let module D = Defaultgraphs.PackageGraph.D in
  let module C = Graph.Components.Make(G) in
  let g = G.create () in
  List.iter (fun pkg ->
      let sn = try Cudf.lookup_package_property pkg "source" with Not_found -> fatal "WTF source" in
      let sv = try Cudf.lookup_package_property pkg "sourcenumber" with Not_found -> fatal "WTF sourcenumber"in
    try
      let src = Cudf.lookup_package universe (CudfAdd.encode ("src"^Src.sep^sn),to_cudf (sn,sv)) in
      (* find an installation set of the build dependencies : the smallest ? *)
      (* get the bin list, add the src package and ask for an
       * installation set of this src in this universe *)
      (* add to the graph all the runtime dependencies that are
       * in this installation set *)
      if not (G.mem_vertex g src) then begin
        let req = Diagnostic_int.Sng (CudfAdd.vartoint universe src) in
        let res = Depsolver_int.solve solver req in
        let closure =
          match res with
          |Diagnostic_int.Success f_int ->
              List.map (CudfAdd.inttovar universe) (f_int ~all:true ())
          |_ -> info "broken source %s %s " sn sv ; []
        in
        List.iter (fun pkg ->
          G.add_vertex g pkg;
          List.iter (fun vpkgs ->
            let l = CudfAdd.resolve_deps universe vpkgs in
            List.iter (fun q ->
              if List.mem q closure then begin
                (* info "%s -> %s" (CudfAdd.string_of_package pkg) (CudfAdd.string_of_package q); *)
                G.add_edge g pkg q
              end;
            ) l;
          ) pkg.Cudf.depends
        ) closure;
      end;
      (* info "%s -> %s" (CudfAdd.string_of_package pkg) (CudfAdd.string_of_package src); *)
      G.add_edge g pkg src;
    with Not_found -> begin
      warning "The source package %s %s associated to the bin package %s is missing" 
      sn sv (CudfAdd.string_of_package pkg);
    end
  ) bl;
  (* this is the graph of all sources and build dependencies and their
   * runtime dependencies *)

  let reduce_bin g =
    List.iter (fun src ->
      if G.mem_vertex g src then
        (* iter on all build-deps of a source *)
        G.iter_succ (fun pkg ->
          (* if the build-dep does not have any dependencies itself
           * then remove the edge *)
          if List.length pkg.Cudf.depends = 0 then
            G.remove_edge g src pkg
        ) g src
    ) sl
  in

  let reduce_src g =
    List.iter (fun bin ->
      if G.mem_vertex g bin then
        match G.succ g bin with
        |[] when G.out_degree g bin = 0 -> G.remove_vertex g bin;
        |[] -> ()
        |l -> 
            List.iter (fun src ->
              if List.mem src sl && G.out_degree g src = 0 then
                begin
                  (* if this binary depends on a source package that does not have
                   * any build dependencies then we can safely remove it from the
                   * graph and recompile it *)
                  Printf.printf "source %s can be compiled\n" (CudfAdd.string_of_package src); 
                  G.remove_edge g bin src;
                  G.remove_vertex g src;
                  if List.length l = 1 then
                    G.remove_vertex g bin
                end
            ) l
    ) bl
  in

  let reduce_loops g =
    let scc = C.scc_array g in
    Array.iter (fun l ->
      let broken = ref false in
      if List.length l > 1 then info "Loop detected";
      List.iter (fun pkg ->
        if not !broken && List.mem pkg sl then begin
          let l = G.succ g pkg in
          let q = List.hd l in
          G.remove_edge g pkg q;
          info "arbitrarly breaking loop between %s and %s" 
          (CudfAdd.string_of_package pkg)
          (CudfAdd.string_of_package q);
          broken := true;
        end
      ) l;
    ) scc;
  in

  let print_dot s g =
    let oc = open_out s in
    D.output_graph oc g;
    close_out oc
  in

  print_dot "reduce-start.dot" g;

  let iteration = ref 0 in
  while G.nb_vertex g > 0 do

    info "Nodes %d (this should descrease)" (G.nb_vertex g);
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

