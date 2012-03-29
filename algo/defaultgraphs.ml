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

(** Specialized Ocamlgraph modules *)

open Graph
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

let tr_timer = Util.Timer.create "Defaultgraph.GraphOper.transitive_reduction"

(** generic operation over imperative graphs *)
module GraphOper (G : Sig.I) = struct

  (** transitive reduction.  Uses the transitive reduction algorithm from The
      Transitive Reduction of a Directed Graph, Aho, Garey and Ullman, 1972 - 
      with the proviso that we know that our graph already is a transitive 
      closure *)
  let transitive_reduction graph =
    Util.Timer.start tr_timer;
    G.iter_vertex (fun v ->
      List.iter (fun v' ->
        if v <> v' then
        List.iter (fun v'' ->
          if v' <> v'' then
            G.remove_edge graph v v''
        ) (G.succ graph v')
      ) (G.succ graph v);
    ) graph;
    Util.Timer.stop tr_timer ()

  module O = Oper.I(G) 


end

(** syntactic dependency graph. Vertex are Cudf packages and
    are indexed considering only the pair name,version .
    Edges are labelled with
    - [OrDepends] : disjuctive dependency
    - [DirDepends] : direct dependecy 
    - [Conflict] : conflict
    *) 
module SyntacticDependencyGraph = struct

  module PkgV = struct
      type t = Pkg of Cudf.package | Or of (Cudf.package * int)
      let compare x y = match (x,y) with
        |Or (p1,i1), Or (p2,i2) when (i1 = i2) && (CudfAdd.equal p1 p2) -> 0
        |Pkg p1, Pkg p2 -> CudfAdd.compare p1 p2
        |_, _ -> Pervasives.compare x y
      let hash = function
        |Pkg p -> Hashtbl.hash (p.Cudf.package,p.Cudf.version)
        |Or (p,i) -> Hashtbl.hash (p.Cudf.package,p.Cudf.version,i)
      let equal x y = match (x,y) with
        |Or (p1,i1), Or (p2,i2) -> (i1 = i2) && (CudfAdd.equal p1 p2)
        |Pkg p1, Pkg p2 -> CudfAdd.equal p1 p2
        |_ -> false
  end

  module PkgE = struct
    type t = OrDepends | DirDepends | Conflict

    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal x y = ((compare x y) = 0)
    let default = DirDepends
  end

  module G = Imperative.Digraph.ConcreteBidirectionalLabeled(PkgV)(PkgE)

  let string_of_vertex vertex =
    match G.V.label vertex with
    |PkgV.Pkg p -> Printf.sprintf "Pkg %s" (CudfAdd.string_of_package p)
    |PkgV.Or (p, _) -> Printf.sprintf "Or %s" (CudfAdd.string_of_package p)

  let string_of_edge edge =
    let label =
      match G.E.label edge with
      |PkgE.DirDepends -> "Direct"
      |PkgE.OrDepends -> "Disjunctive"
      |PkgE.Conflict -> "Conflict"
    in
    let src = G.E.src edge in
    let dst = G.E.dst edge in
    Printf.sprintf "%s %s %s"
    (string_of_vertex src)
    label
    (string_of_vertex dst)

  module Display = struct
      include G
      let vertex_name v =
        match G.V.label v with
        |PkgV.Pkg i -> Printf.sprintf "\"%s\"" (CudfAdd.string_of_package i)
        |PkgV.Or (i,c) -> Printf.sprintf "\"Or%s-%d\"" (CudfAdd.string_of_package i) c

      let graph_attributes = fun _ -> [`Rankdir `LeftToRight]
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> [`Shape `Box]

      let vertex_attributes v =
        match G.V.label v with
        |PkgV.Or _ -> [`Label "Or" ; `Shape `Diamond]
        |_ -> []

      let edge_attributes e =
        match G.E.label e with
        |PkgE.DirDepends -> [`Style `Solid]
        |PkgE.OrDepends -> [`Style `Dashed]
        |PkgE.Conflict -> [`Color 0xFF0000; `Style `Solid; `Label "#"]
    end

  (** Graphviz outoput module *)
  module D = Graph.Graphviz.Dot(Display) 
  module S = Set.Make(PkgV)

  let depgraphbar = Util.Progress.create "SyntacticDependencyGraph.dependency_graph"

  (** Build the syntactic dependency graph from the give cudf universe *)
  let dependency_graph univ =
    let timer = Util.Timer.create "SyntacticDependencyGraph.dependency_graph" in
    Util.Timer.start timer;
    let conflicts = CudfAdd.init_conflicts univ in
    Util.Progress.set_total depgraphbar (Cudf.universe_size univ);
    let gr = G.create () in
    Cudf.iter_packages (fun pkg ->
      Util.Progress.progress depgraphbar;
      let vpid = G.V.create (PkgV.Pkg pkg) in
      let c = ref 0 in
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_deps univ vpkgs with 
        |[] -> ()
        |[p] ->
            let vp = G.V.create (PkgV.Pkg p) in
            let edge = G.E.create vpid PkgE.DirDepends vp in
            G.add_edge_e gr edge
        |l ->
            begin
              let vor = G.V.create (PkgV.Or (pkg,!c)) in
              let edgeor = G.E.create vpid PkgE.OrDepends vor in
              G.add_edge_e gr edgeor;
              incr c;
              List.iter (fun p ->
                let vp = G.V.create (PkgV.Pkg p) in
                let oredge = G.E.create vor PkgE.OrDepends vp in
                G.add_edge_e gr oredge
              ) l
            end
      ) pkg.Cudf.depends
      ;
      List.iter (fun p ->
        if not(CudfAdd.equal p pkg) then
          let vp = G.V.create (PkgV.Pkg p) in
          let edge = G.E.create vpid PkgE.Conflict vp in
          G.add_edge_e gr edge
      ) (CudfAdd.who_conflicts conflicts univ pkg)
    ) univ
    ;
    Util.Timer.stop timer gr
  ;;

end

(******************************************************)

(** Imperative bidirectional graph for dependecies. *)
(** Imperative unidirectional graph for conflicts. *)
module MakePackageGraph(V : Sig.COMPARABLE with type t = Cudf.package )(E : Sig.ORDERED_TYPE_DFT) = struct

  module PkgV = V
  module PkgE = E
  module G = Imperative.Digraph.ConcreteBidirectionalLabeled(PkgV)(PkgE)
  module UG = Imperative.Graph.Concrete(PkgV)
  module O = GraphOper(G)
  module S = Set.Make(PkgV)

  module DisplayF (G : Sig.I) =
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (CudfAdd.string_of_package v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v = []

      let edge_attributes e = []
    end
  module Display = DisplayF(G)
  module D = Graph.Graphviz.Dot(Display)

  (** Build the dependency graph from the given cudf universe *)
  let dependency_graph ?(conjunctive=false) universe =
    let gr = G.create () in
    Cudf.iter_packages (fun pkg ->
      G.add_vertex gr pkg;
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_deps universe vpkgs with
        |[p] -> G.add_edge gr pkg p
        |l when not conjunctive -> List.iter (G.add_edge gr pkg) l
        |_ -> ()
      ) pkg.Cudf.depends
    ) universe
    ;
    gr

  (** Build the dependency graph from the given list of packages *)
  let dependency_graph_list ?(conjunctive=false) universe pkglist =
    let gr = G.create () in
    List.iter (fun pkg ->
      G.add_vertex gr pkg;
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_deps universe vpkgs with
        |[p] -> G.add_edge gr pkg p
        |l when not conjunctive -> List.iter (G.add_edge gr pkg) l
        |_ -> ()
      ) pkg.Cudf.depends
    ) pkglist
    ;
    gr

  (** Build the conflict graph from the given cudf universe *)
  let conflict_graph universe =
    let gr = UG.create () in
    Cudf.iter_packages (fun pkg ->
      List.iter (fun (pkgname,constr) ->
        List.iter (UG.add_edge gr pkg)
        (CudfAdd.who_provides universe (pkgname,constr))
      ) pkg.Cudf.conflicts
    ) universe
    ;
    gr

  let undirect graph =
    let gr = UG.create () in
    G.iter_edges (UG.add_edge gr) graph;
    G.iter_vertex (UG.add_vertex gr) graph;
    gr

  (** Return the list of connected component graphs *)
  let connected_components graph =
    let module Dfs = Graph.Traverse.Dfs(UG) in
    let cc graph mark id =
      let g = UG.create () in
      let collect v1 = 
        Hashtbl.add mark v1 () ; 
        UG.iter_succ (fun v2 -> UG.add_edge g v1 v2) graph v1
      in
      Dfs.prefix_component collect graph id;
      g
    in
    let mark = Hashtbl.create (UG.nb_vertex graph) in
    UG.fold_vertex (fun v acc ->
      if not(Hashtbl.mem mark v) then (cc graph mark v)::acc else acc
    ) graph []

  let out ?(dump=None) ?(dot=None) ?(detrans=false) pkggraph =
    info "Dumping Graph : nodes %d , edges %d"
    (G.nb_vertex pkggraph) (G.nb_edges pkggraph) ;
    
    if detrans then begin
      O.transitive_reduction pkggraph;
      debug "After transitive reduction : nodes %d , edges %d"
      (G.nb_vertex pkggraph) (G.nb_edges pkggraph)
    end ;

    if dump <> None then begin
      let f = Option.get dump in
      debug "Saving marshal graph in %s\n" f ;
      let oc = open_out f in
      Marshal.to_channel oc ((detrans,pkggraph) :> (bool * G.t)) [];
      close_out oc
    end ;

    if dot <> None then begin
      let f = Option.get dot in
      debug "Saving dot graph in %s\n" f ;
      let oc = open_out f in
      D.output_graph oc pkggraph;
      close_out oc
    end 

end

(******************************************************)

module PkgV = struct
    type t = Cudf.package
    let compare = CudfAdd.compare
    let hash = CudfAdd.hash
    let equal = CudfAdd.equal
end

module PkgE = struct
  type t = float
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0.0
end

module PackageGraph = MakePackageGraph(PkgV)(PkgE)

(** Integer Imperative Bidirectional Graph *)
module IntPkgGraph = struct

  module PkgV = struct
      type t = int
      let compare = Pervasives.compare
      let hash i = i
      let equal = (=)
  end

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)
  module S = Set.Make(PkgV)
  module O = GraphOper(G)

  module Display =
    struct
      include G
      let vertex_name uid = Printf.sprintf "\"%d\"" uid

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v = []

      let edge_attributes e = []
    end

  module D = Graph.Graphviz.Dot(Display)

  module DIn = Dot.Parse (Builder.I(G))(
    struct
      let node (id,_) _ =
        match id with
        |Graph.Dot_ast.String s -> int_of_string s
        |_ -> assert false
      let edge _ = ()
    end
  )

  let add_edge transitive graph i j =
    let rec adapt k red =
      let new_red = 
        S.fold (fun l acc ->
          if k <> l then G.add_edge graph k l;
          G.fold_succ (fun m acc' ->
            if not (G.mem_edge graph k m) 
            then S.add m acc'
            else acc'
          ) graph l acc
        ) red S.empty 
      in
      if S.is_empty new_red then ()
      else adapt k new_red
    in
  begin
    debug "Adding edge from %d to %d" i j;
    G.add_edge graph i j;
    if transitive then begin
      adapt i (S.singleton j);
      G.iter_pred (fun k ->
        if not (G.mem_edge graph k j) then
          adapt k (S.singleton j)
      ) graph i
    end
  end

  (** add to the graph all conjunctive dependencies of package id *)
  let conjdepgraph_int ?(transitive=false) graph univ id =
    G.add_vertex graph id;
    let p = CudfAdd.inttovar univ id in
    List.iter (fun vpkgs ->
      match CudfAdd.resolve_vpkgs_int univ vpkgs with
      |[q] when q <> id -> add_edge transitive graph id q
      |_ -> ()
    ) p.Cudf.depends

  (** for all id \in idlist add to the graph all conjunctive dependencies *)
  let conjdepgraph univ idlist =
    let graph = G.create ~size:(List.length idlist) () in
    List.iter (conjdepgraph_int graph univ) idlist;
    graph

  (** given a graph return the conjunctive dependency closure of the package id *)
  let conjdeps graph =
    let h = Hashtbl.create (G.nb_vertex graph) in
    fun id ->
      try Hashtbl.find h id
      with Not_found -> begin
        let module Dfs = Traverse.Dfs(G) in
        let l = ref [] in
        let collect id = l := id :: !l in
        Dfs.prefix_component collect graph id;
        Hashtbl.add h id !l;
        !l
      end

  (** Build the dependency graph from the given index. conjunctive and
      disjunctive dependencies are considered as equal *)
  let dependency_graph ?(conjunctive=false) universe =
    let size = Cudf.universe_size universe in
    let graph = G.create ~size () in
    Cudf.iteri_packages (fun id pkg ->
      G.add_vertex graph id;
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_vpkgs_int universe vpkgs with
        |[p] -> G.add_edge graph id p
        |l when not conjunctive -> List.iter (G.add_edge graph id) l
        |_ -> ()
      ) pkg.Cudf.depends
    ) universe;
    graph

  let dependency_graph_list ?(conjunctive=false) universe idlist =
    let queue = Queue.create () in
    let graph = G.create () in
    let visited = Hashtbl.create (2 * (List.length idlist)) in
    List.iter (fun e -> Queue.add e queue) idlist;
    while (Queue.length queue > 0) do
      let id = Queue.take queue in
      let pkg = Cudf.package_by_uid universe id in
      if not(Hashtbl.mem visited id) then begin
        G.add_vertex graph id;
        Hashtbl.add visited id ();
        List.iter (fun vpkgs ->
          match CudfAdd.resolve_vpkgs_int universe vpkgs with
          |[i] when not(Hashtbl.mem visited i) -> begin
              Queue.add i queue;
              G.add_edge graph id i
          end
          |dsj when not conjunctive ->
            List.iter (fun i ->
              if not(Hashtbl.mem visited i) then begin
                Queue.add i queue;
                G.add_edge graph id i
              end
            ) dsj
          |_ -> ()
        ) pkg.Cudf.depends
      end
    done;
    graph
;;

  let load pkglist filename =
    let timer = Util.Timer.create "Defaultgraph.StrongDepGraph.load" in
    Util.Timer.start timer;
    let ic = open_in filename in
    let (detrans,graph) = ((Marshal.from_channel ic) :> (bool * G.t)) in
    close_in ic ;
    info "Loading Strong Dependencies graph";
    (* we assume the graph is detransitivitized *)
    let sg =
      if detrans then begin 
        info "Computing transitive closure";
        (* O.add_transitive_closure graph *) 
        graph
      end else graph
    in
    Util.Timer.stop timer sg

end

let intcudf universe intgraph =
  let module PG = PackageGraph.G in
  let module SG = IntPkgGraph.G in
  let trasformtimer = Util.Timer.create "Defaultgraphs.intcudf" in
  Util.Timer.start trasformtimer;
  let size = 25000 in
  let cudfgraph = PG.create ~size () in
  SG.iter_edges (fun x y ->
    let p = CudfAdd.inttovar universe x in
    let q = CudfAdd.inttovar universe y in
    PG.add_edge cudfgraph p q
  ) intgraph ;
  SG.iter_vertex (fun v ->
    let p = CudfAdd.inttovar universe v in
    PG.add_vertex cudfgraph p
  ) intgraph ;
  debug "cudfgraph: nodes %d , edges %d"
  (PG.nb_vertex cudfgraph) (PG.nb_edges cudfgraph);
  Util.Timer.stop trasformtimer cudfgraph

(** transform a cudf graph into a integer graph *)
let cudfint universe cudfgraph =
  let module PG = PackageGraph.G in
  let module SG = IntPkgGraph.G in
  let trasformtimer = Util.Timer.create "DefaultGraphs.cudfint" in
  Util.Timer.start trasformtimer;
  let intgraph = SG.create () in
  PG.iter_edges (fun x y ->
    SG.add_edge intgraph
    (CudfAdd.vartoint universe x)
    (CudfAdd.vartoint universe y)
  ) cudfgraph;
  PG.iter_vertex (fun v ->
    SG.add_vertex intgraph (CudfAdd.vartoint universe v)
  ) cudfgraph;
  debug "intcudf: nodes %d , edges %d"
  (SG.nb_vertex intgraph) (SG.nb_edges intgraph);
  Util.Timer.stop trasformtimer intgraph

