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

type edge = (Cudf.package * constr )
and constr = Hard | Soft of int
module PkgE = struct
  type t = edge ref
  let compare (x :t) (y :t) =
    let (p1,c1) = !x and (p2,c2) = !y in
    if CudfAdd.equal p1 p2 then 0 else
    match c1,c2 with
    |(Hard,Hard) -> 0
    |(Hard,Soft _ ) -> 1
    |(Soft _ ,Hard) -> -1
    |(Soft a, Soft b) -> Pervasives.compare a b
  let hash x = Hashtbl.hash !x
  let equal x y = !x = !y
  let default = ref (Cudf.default_package,Hard)
end

module PG = Defaultgraphs.MakePackageGraph(Defaultgraphs.PkgV)(PkgE)

module G = PG.G
module D = PG.D
module C = Graph.Components.Make(G)
module Dfs = Graph.Traverse.Dfs(G)
module O = Defaultgraphs.GraphOper(G)

module SV = Set.Make(G.V)
module SE = Set.Make(G.E)

let partition s w = snd(SV.partition (fun e -> e >= w) s) ;;
let to_set l = List.fold_right SV.add l SV.empty ;;

let edge_label_string = function
  |(p,Hard) ->
    Printf.sprintf "(%s,Hard)"
    (CudfAdd.string_of_package p)
  |(p,Soft i) -> 
    Printf.sprintf "(%s,%d)"
    (CudfAdd.string_of_package p)
    i
;;

let edge_to_string (s,l,d) =
  Printf.sprintf "%s -%s-> %s"
  (CudfAdd.string_of_package s)
  (edge_label_string !l)
  (CudfAdd.string_of_package d)
;;

let print_edge_list_e s l =
    Printf.printf "%s: %s\n" s (String.join " " (
      List.map edge_to_string l)
    )
;;

let print_dot s g =
  let oc = open_out s in
  D.output_graph oc g;
  close_out oc
;;

(* ------------------------------------------ *)

let hash_to_list t = Hashtbl.fold (fun k _ acc -> k::acc) t [] ;;

(* ------------------------------------------ *)

(* returns a new graph containg a copy of all edges and vertex of g *)
let copy_graph g =
  let g1 = G.create () in
  G.iter_edges_e (fun e -> G.add_edge_e g1 e) g;
  G.iter_vertex (fun v -> G.add_vertex g1 v) g;
  g1
;;

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
;;

(* return a new graph without all the edges in l - as G.E.t list *)
let remove_edges_e g l =
  let sg = copy_graph g in
  List.iter (G.remove_edge_e sg) l;
  sg
;;


(* ------------------------------------------ *)

type block = {
  blocked : (G.V.t,bool) Hashtbl.t;
  notelem : (G.V.t,G.V.t list) Hashtbl.t
}

let init_block g =
  let t = {
    blocked = Hashtbl.create 1023;
    notelem = Hashtbl.create 1023;
  } in
  G.iter_vertex (fun node ->
    Hashtbl.add t.blocked node false;
    Hashtbl.add t.notelem node [];
  ) g;
  t
;;

let get_notelem t n =
  try Hashtbl.find t.notelem n with Not_found -> []
;;

let is_bloked t n =
  try Hashtbl.find t.blocked n with Not_found -> false
;;

let rec unblock t n =
  if is_bloked t n then begin
    Hashtbl.replace t.blocked n false;
    let l = get_notelem t n in
    List.iter (unblock t) l;
    Hashtbl.replace t.notelem n []
  end
;;

let block t n =
  Hashtbl.replace t.blocked n true
;;

(* ------------------------------------------ *)

(* return one cycle in g, if one exists *)
let find_min_cycle g =
  let min_weigth l =
    List.fold_left (fun acc e ->
      match snd(!(G.E.label e)) with
      |Hard -> acc
      |Soft i -> acc+i
      ) 0 l
  in

  let rec circuit path t thisnode startnode component =
     let rec aux acc = function
       |[] -> acc
       |edge :: rest ->
           let nextnode = G.E.dst edge in
           print_edge_list_e "candidate " (edge::path);
           Printf.printf "min weight path %d\n" (min_weigth (edge::path));
           if G.V.equal nextnode startnode then begin
             unblock t thisnode;
             match acc with
             |None -> aux (Some(List.rev (edge::path))) rest
             |Some p when min_weigth (edge::path) >= min_weigth p ->
                 print_edge_list_e "min so far " p;
                 Printf.printf "min weight so far %d\n" (min_weigth p);
                 aux (Some(List.rev (edge::path))) rest
             |_ -> aux acc rest
           end else
             if not(is_bloked t nextnode) then begin
               let e = circuit (edge::path) t nextnode startnode component in
               match acc,e with
               |None,_ -> aux e rest
               |Some p,Some path when min_weigth path >= min_weigth p -> begin
                 print_edge_list_e "min so far " p;
                 Printf.printf "min weight so far %d\n" (min_weigth p);
                 aux e rest
               end
               |_,_ -> aux acc rest
             end else begin
               aux acc rest
             end
     in
     block t thisnode;
     let e = aux None (G.succ_e component thisnode) in
     G.iter_succ (fun nextnode ->
       let l = get_notelem t nextnode in
       if List.mem thisnode l then
         Hashtbl.replace t.notelem nextnode (thisnode::l)
     ) component thisnode;
     e
  in
  let vertex_set = G.fold_vertex SV.add g SV.empty in
  let rec aux acc = function
    |[] -> acc
    |s :: rest ->
      let subset = SV.add s (partition vertex_set s) in
      let subgraph = extract_subgraph g subset in
      (* I need only one... not all scc *)
      (* actually I need only the scc that contains the min *)
      let scc = C.scc_list subgraph in
      let minnode = SV.min_elt subset in
      let mincomp = List.find (fun l -> List.mem minnode l) scc in
      let startnode = minnode in
      let component = extract_subgraph subgraph (to_set mincomp) in
      let t = init_block component in
      match acc,circuit [] t startnode startnode component with
      |None,e -> aux e rest
      |Some p,Some path when min_weigth path >= min_weigth p -> aux (Some path) rest
      |_,_ -> aux acc rest
  in
  match aux None (SV.elements vertex_set) with
  |None -> SE.empty
  |Some c ->
  List.fold_right SE.add c SE.empty
;;

(* ------------------------------------------ *)

(* return a feedback arc set of a weight graph *)
let fas sg =
  let g = copy_graph sg in
  (* f is a set of edges to remove *)
  let f = Hashtbl.create 1023 in
  let weight e = !(G.E.label e) in
  let update_weight e w = 
    let l = G.E.label e in 
    match (weight e),!l with
    |(p,Soft i),(_,Soft j) ->  l := (p,Soft (j -i))
    |_,_ -> ()
  in
  let add k = Hashtbl.replace f k () in
  let subgraph = ref g in
  let is_stuck = ref (Hashtbl.length f) in
  while Dfs.has_cycle !subgraph do
    Util.Progress.progress progressbar_u;
    let c = find_min_cycle !subgraph in
    print_edge_list_e "simple cycle" (SE.elements c);

    (* edge with min weight *)
    let eps = weight (SE.min_elt c) in
    Printf.printf "min %s\n%!" (edge_label_string eps) ;
    SE.iter (fun e ->
      Printf.printf "w e %s\n%!" (edge_label_string (weight e)) ;
      update_weight e eps;
      Printf.printf "update %s\n%!" (edge_label_string (weight e)) ;
      Printf.printf "Candidate to be removed %s \n%!" (edge_to_string e);
      add e
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
;;

(* ------------------------------------------ *)

let create_source_graph tables (bl,sl,pkglist) universe =
  let is_build_essential pkg =
    try bool_of_string (Cudf.lookup_package_property pkg "buildessential")
    with Not_found -> false
  in

  let is_essential pkg =
    try bool_of_string (Cudf.lookup_package_property pkg "essential")
    with Not_found -> false
  in

  let get_source tables pkg =
    let to_cudf = Debcudf.get_cudf_version tables in
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
      let constr =
        if is_build_essential bin || is_essential bin then Hard
        else
          if is_direct_build_dep src bin then
            if is_doc bin then Soft(2) else
              if is_lib bin then Soft (1) else Hard
          else Hard
      in ref (bin,constr)
    in

    let solver = Depsolver_int.init_solver_univ universe in
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
      let label = assign_label src bin in
      let dst = get_source tables bin in
      if not(CudfAdd.equal src dst) then begin
        let e = G.E.create src label dst in
        info "Add Edge %s" (edge_to_string e);
        G.add_edge_e g e
      end
    ) closure
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
      let src = get_source tables bin in
      begin try add_source g src with Not_found -> () end;
    with Not_found -> () (* from get_source *)
  ) bl ;
  g
;;

let main () =

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
  let to_cudf = Debcudf.get_cudf_version tables in
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

  let universe = Cudf.load_universe pkglist in

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

  if OptParse.Opt.is_set Options.dump then begin
    let oc = open_out (OptParse.Opt.get Options.dump) in
    info "Dumping Cudf file";
    Cudf_printer.pp_preamble oc Debcudf.preamble;
    Printf.fprintf oc "\n";
    Cudf_printer.pp_universe oc universe
  end;
  let g = create_source_graph tables (bl,sl,pkglist) universe in
  print_dot "source.dot" g;
  let e = find_min_cycle g in
  print_edge_list_e "min cycle" (SE.elements e);
  let el = fas g in
  print_edge_list_e "fas" el;
  let sg = remove_edges_e g el in
  print_dot "fas.dot" sg
;;

main ();;
