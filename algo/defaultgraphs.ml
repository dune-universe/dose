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

open ExtLib
open Graph
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

let tr_timer = Util.Timer.create "Defaultgraph.GraphOper.transitive_reduction"
let trbar = Util.Progress.create "Defaultgraph.GraphOper.transitive_reduction"

(** generic operation over imperative graphs *)
module GraphOper (G : Sig.I) = struct

  (** transitive reduction.  Uses the transitive reduction algorithm from The
      Transitive Reduction of a Directed Graph, Aho, Garey and Ullman, 1972 - 
      with the proviso that we know that our graph already is a transitive 
      closure *)
  (* this is a VERY expensive operation on Labelled graphs ... *)
  let transitive_reduction graph =
    Util.Progress.set_total trbar (G.nb_vertex graph);
    Util.Timer.start tr_timer;
    G.iter_vertex (fun v ->
      Util.Progress.progress trbar;
      G.iter_succ (fun w ->
        if not(G.V.equal v w) then
        G.iter_succ (fun z ->
          if not(G.V.equal w z) then
            G.remove_edge graph v z
        ) graph w
      ) graph v;
    ) graph;
    Util.Timer.stop tr_timer ();
    Util.Progress.reset trbar
  ;;

  module O = Oper.I(G) 
  module S = Set.Make(G.V)

  (** extract the subgraph induced by the list l *)
  let subgraph g l =
    let to_set l = List.fold_left (fun s v -> S.add v s) S.empty l in
    let s = to_set l in
    let sg = G.create () in
    S.iter (fun v1 ->
      G.add_vertex sg v1;
      List.iter (fun e ->
        let v2 = G.E.dst e in
        if S.mem v2 s then
          G.add_edge_e sg e
      ) (G.succ_e g v1)
    ) s;
    sg
  ;;

end

(** Syntactic dependency graph. Vertices are cudf packages, 
    OR nodes representing a disjunctive dependency, or Missing nodes
    representing a missing package. The latter is used to display explanation
    graphs. Vertices are indexed considering only the pair name,version .
    Edges are labelled with
    - [OrDepends] : disjuctive dependency
    - [DirDepends] : direct dependecy 
    - [Conflict] : conflict
    *) 
module SyntacticDependencyGraph = struct 
  type pkg = { value : Cudf.package; root : bool}

  module PkgV = struct
    type t = 
      |Pkg of pkg
      |Set of CudfAdd.Cudf_set.t
      |Or of int
      |Missing of Cudf_types.vpkglist
    let compare x y = match (x,y) with
      |Or i1, Or i2 -> (i1 - i2)
      |Pkg {value = p1}, Pkg {value = p2} -> CudfAdd.compare p1 p2
      |Set s1, Set s2 -> CudfAdd.Cudf_set.compare s1 s2
      |_, _ -> Pervasives.compare x y
    let hash = function
      |Pkg {value = p} -> Hashtbl.hash (p.Cudf.package,p.Cudf.version)
      |Set s -> Hashtbl.hash s (* XXX Can fail ! *)
      |Or i -> Hashtbl.hash i
      |Missing i -> Hashtbl.hash i (* XXX *)
    let equal x y = match (x,y) with
      |Or i1, Or i2 -> (i1 = i2)
      |Pkg {value = p1}, Pkg {value = p2} -> CudfAdd.equal p1 p2
      |Set s1, Set s2 -> CudfAdd.Cudf_set.equal s1 s2
      |Missing i, Missing j -> i = j (* XXX *)
      |_ -> false
  end

  module PkgE = struct
    type s =
      |OrDepends of Cudf_types.vpkglist
      |DirDepends of Cudf_types.vpkglist
      |MissingDepends of Cudf_types.vpkglist
      |Conflict
    type t = s ref

    let compare x y = Pervasives.compare !x !y
    let hash x = Hashtbl.hash !x
    let equal x y = ((compare x y) = 0)
    let default = ref Conflict
  end

  module G = Imperative.Digraph.ConcreteBidirectionalLabeled(PkgV)(PkgE)
  module DotPrinter = struct
    module Display = struct
      include G
      let vertex_name v = string_of_int (G.V.hash v)

      let graph_attributes = fun _ -> [`Rankdir `LeftToRight]
      let get_subgraph v = None
        (*
        let open Graphviz.DotAttributes in
        match G.V.label v with
        |PkgV.Pkg {value = p} -> Some 
          { sg_name = string_of_int (Hashtbl.hash p.Cudf.package);
            sg_attributes = [`Style `Invis];
            sg_parent = None }
        |_ -> None
        *)

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> [`Shape `Box]

      let vertex_attributes v =
        match G.V.label v with
        |PkgV.Or i -> [`Label "Or"; `Shape `Diamond]
        |PkgV.Pkg {value = p; root = r} -> 
            let al = ref [`Label (CudfAdd.string_of_package p)] in
            if p.Cudf.installed then al := (`Color 0x00FF00)::!al;
            if r then al := (`Shape `Record)::!al;
            !al
        |PkgV.Set s -> 
            let l = CudfAdd.Cudf_set.elements s in
            let p = CudfAdd.Cudf_set.choose s in
            let str = 
              Printf.sprintf "\"%s (=%s)\""
              (CudfAdd.decode p.Cudf.package)
              (Util.string_of_list ~delim:("[","]") CudfAdd.string_of_version l)
            in
            [`Label str; `Shape `Record]
        |PkgV.Missing _ -> [ `Label "Missing"; `Color 0x00FF00; `Shape `Ellipse]

      let edge_attributes e =
        match !(G.E.label e) with
        |PkgE.DirDepends _ -> [`Style `Solid]
        |PkgE.OrDepends _ -> [`Style `Dashed]
        |PkgE.MissingDepends vpkgs ->
            [`Style `Solid ; `Label (CudfAdd.decode (Cudf_types_pp.string_of_vpkglist vpkgs))]
        |PkgE.Conflict -> [`Color 0xFF0000; `Style `Solid; `Dir `None; `Label "#"]
    end

    include Graph.Graphviz.Dot(Display)
    let print fmt g = fprint_graph fmt g
  end 
  module S = Set.Make(PkgV)

  module GmlPrinter = Gml.Print (G) (
    struct
      let node (v: G.V.label) = []
      let edge (e: G.E.label) = []
    end)

  module GraphmlPrinter = Graphml.Print (G) (
    struct
      let vertex_properties =
          ["package","string",None;
           "version","string",None;
           "architecture","string",None;
           "type","string",None;
           "source","string",None;
           "sourcenumber","string",None;
           "multiarch","string",None;
          ]
     
      let edge_properties = [
        "vpkglist","string",None;
        "binaries","string",None;
        ]

      let map_edge e = []
      let map_vertex = function
        |PkgV.Pkg {value = p} ->
          let name = ("package",CudfAdd.decode p.Cudf.package) in
          let version = ("version",CudfAdd.string_of_version p) in
     
          let props =
            List.filter_map (fun (key,_,_) ->
              try let value = Cudf.lookup_package_property p key in
              Some(key,value)
              with Not_found -> None
            ) vertex_properties
          in
          name :: version :: props
        |PkgV.Set _ -> [] (* XXX *)
        |PkgV.Or _ -> []
        |PkgV.Missing _ -> []

      let edge_uid e = Hashtbl.hash e
      let vertex_uid v = Hashtbl.hash v

    end)

  let depgraphbar = Util.Progress.create "SyntacticDependencyGraph.dependency_graph"

  let add_edge gr s label d =
    try
      let e = G.find_edge gr s d in
      let labelold = G.E.label e in
      match !labelold, label with
      |PkgE.MissingDepends vpkgs1,PkgE.MissingDepends vpkgs2 ->
          labelold := (PkgE.MissingDepends (vpkgs1@vpkgs2))
      |PkgE.DirDepends vpkgs1, PkgE.DirDepends vpkgs2 ->
          labelold := (PkgE.DirDepends (vpkgs1@vpkgs2))
      |PkgE.OrDepends vpkgs1, PkgE.OrDepends vpkgs2 ->
          labelold := (PkgE.OrDepends (vpkgs1@vpkgs2))
      |_,_-> G.add_edge_e gr (G.E.create s (ref label) d)
    with Not_found ->
      G.add_edge_e gr (G.E.create s (ref label) d)

  (** Build the syntactic dependency graph from the give cudf universe *)
  let dependency_graph ?root univ =
    let add_node p =
      let r = 
        match root with
        |None -> false
        |Some pkg -> CudfAdd.equal p pkg
      in
      G.V.create (PkgV.Pkg {value = p; root = r})
    in
    let timer = Util.Timer.create "SyntacticDependencyGraph.dependency_graph" in
    Util.Timer.start timer;
    let conflicts = CudfAdd.init_conflicts univ in
    Util.Progress.set_total depgraphbar (Cudf.universe_size univ);
    let gr = G.create () in
    let c = ref 0 in
    Cudf.iter_packages (fun pkg ->
      Util.Progress.progress depgraphbar;
      let vpid = add_node pkg in
      G.add_vertex gr vpid;
      List.iter (fun vpkgs ->
        match CudfAdd.resolve_deps univ vpkgs with 
        |[] ->
            let vp = G.V.create (PkgV.Missing vpkgs) in
            add_edge gr vpid (PkgE.MissingDepends vpkgs) vp
        |[p] ->
            let vp = add_node p in
            add_edge gr vpid (PkgE.DirDepends vpkgs) vp
        |l ->
            begin
              let vor = G.V.create (PkgV.Or !c) in incr c;
              add_edge gr vpid (PkgE.OrDepends vpkgs) vor;
              List.iter (fun p ->
                let vp = add_node p in
                add_edge gr vor (PkgE.OrDepends vpkgs) vp
              ) l
            end
      ) pkg.Cudf.depends
      ;
      List.iter (fun p ->
        if not(CudfAdd.equal p pkg) then
          let vp = add_node p in
          let edge = G.E.create vpid (ref PkgE.Conflict) vp in
          G.add_edge_e gr edge
      ) (CudfAdd.who_conflicts conflicts univ pkg)
    ) univ
    ;
    Util.Timer.stop timer gr
  ;;

  let pp v = 
    match G.V.label v with
    |PkgV.Or i -> "Or"
    |PkgV.Pkg {value = p; root = r} -> 
        (CudfAdd.string_of_package p)
    |PkgV.Set s -> 
        let l = CudfAdd.Cudf_set.elements s in
        let p = CudfAdd.Cudf_set.choose s in
        Printf.sprintf "\"%s (=%s)\""
        (CudfAdd.decode p.Cudf.package)
        (Util.string_of_list ~delim:("[","]") CudfAdd.string_of_version l)
    |PkgV.Missing _ -> "Missing"

end

(******************************************************)

(** Imperative bidirectional graph for dependecies. *)
(** Imperative unidirectional graph for conflicts. *)
(* Note: ConcreteBidirectionalLabelled graphs are slower and we do not use them
 * here *)
module MakePackageGraph(PkgV : Sig.COMPARABLE with type t = Cudf.package )= struct

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)
  module UG = Imperative.Graph.Concrete(PkgV)
  module O = GraphOper(G)
  module S = Set.Make(PkgV)

  module DotPrinter = struct
  module Display = struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (CudfAdd.string_of_package v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes p =
        if p.Cudf.installed then [ `Color 0x00FF00 ] else []

      let edge_attributes e = []
    end
    include Graph.Graphviz.Dot(Display)
    let print fmt g = fprint_graph fmt g
  end 

  module GmlPrinter = Gml.Print (G) (
    struct
       let node (v: G.V.label) = []
       let edge (e: G.E.label) = []
     end
  )

  module GraphmlPrinter = Graphml.Print (G)(
    struct
        let vertex_properties =
            ["package","string",None;
             "version","string",None;
             "architecture","string",None;
             "type","string",None;
             "source","string",None;
             "sourcenumber","string",None;
             "multiarch","string",None;
            ]
       
        let edge_properties = [
          "vpkglist","string",None;
          "binaries","string",None;
          ]

        let map_edge e = []
        let map_vertex pkg =
          let name = ("package",CudfAdd.decode pkg.Cudf.package) in
          let version = ("version",CudfAdd.string_of_version pkg) in
     
          let props =
            List.filter_map (fun (key,_,_) ->
              try let value = Cudf.lookup_package_property pkg key in
              Some(key,value)
              with Not_found -> None
            ) vertex_properties
          in
          name :: version :: props

        let edge_uid e = Hashtbl.hash e
        let vertex_uid v = Hashtbl.hash v

    end)

  (* Maintenance Of Transitive Closures And Transitive Reductions Of Graphs *)
  (* J.A. La Poutre and J. van Leeuwen *)
  let add_edge ?transitive graph i j =
    let rec adapt g k red =
      let new_red = 
        S.fold (fun l acc ->
          if not(G.V.equal k l) then G.add_edge g k l;
          G.fold_succ (fun m acc' ->
            if not (G.mem_edge g k m) then S.add m acc'
            else acc'
          ) g l acc
        ) red S.empty 
      in
      if S.is_empty new_red then ()
      else adapt g k new_red
    in
    let insert g i j =
      adapt g i (S.singleton j);
      G.iter_pred (fun k ->
        if not (G.mem_edge g k j) then
          adapt g k (S.singleton j)
      ) g i
    in
    match transitive with
    |None -> G.add_edge graph i j
    |Some true -> 
      (* add an edge and maintain the transitive clousure of the graph *)
      insert graph i j 
    |Some false ->
      (* TODO : add an edge and maintain the transitive reduction of the graph *)
      G.add_edge graph i j

  (** add to the graph all conjunctive dependencies of package id *)
  let conjdepgraph_int ?(transitive=false) graph univ p =
    G.add_vertex graph p;
    List.iter (fun vpkgs ->
      match CudfAdd.resolve_deps univ vpkgs with
      |[q] when not(CudfAdd.equal q p) -> add_edge ~transitive graph p q
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

  (** Return the list of connected component of an undirected graph *)
  let connected_components graph =
    let module C = Graph.Components.Make(UG) in
    C.scc_list graph

  let pred_list graph q =
    G.fold_pred (fun p acc -> p :: acc ) graph q []

  let succ_list graph q =
    G.fold_succ (fun p acc -> p :: acc ) graph q []

  let pred_set graph q =
    if G.mem_vertex graph q then
      G.fold_pred (fun p acc -> S.add p acc) graph q S.empty
    else S.empty

  let succ_set graph q =
    if G.mem_vertex graph q then
      G.fold_succ (fun p acc -> S.add p acc) graph q S.empty
    else S.empty

  let cycle_reduction g =
    let module Hashtbl = CudfAdd.Cudf_hashtbl in
    let module Set = CudfAdd.Cudf_set in
    let visited = Hashtbl.create (G.nb_vertex g) in
    let rec get_cycle res path v =
      match path with
      |[] -> fatal "No cycle in path!"
      |h::t when G.V.equal h v -> (t, res)
      |h::t -> get_cycle (h::res) t v
    in
    let reduce_cycle path v =
      (* Somewhere, v should be in path. This is the cycle. *)
      let (other, c) = get_cycle [] path v in
      let nv = 
        let name = String.concat "/" (List.sort ~cmp:compare (List.map (fun p -> p.Cudf.package) (v::c))) in
        { Cudf.default_package with
          Cudf.package = CudfAdd.encode name;
          Cudf.version = 1;
        }
      in
      G.add_vertex g nv;
      let s = CudfAdd.to_set c in
      List.iter (fun p ->
        if G.mem_vertex g p then begin
          G.iter_pred (fun q -> if not (Set.mem q s) then G.add_edge g q nv) g p;
          G.iter_succ (fun q -> if not (Set.mem q s) then G.add_edge g nv q) g p;
          G.remove_vertex g p;
        end;
        Hashtbl.remove visited p
      ) (v::c);
      (other, nv)
    in
    let rec visit path v =
      if G.mem_vertex g v then begin
        Hashtbl.add visited v true;
        G.iter_succ (fun w ->
          try
            if Hashtbl.find visited w then
              let (other, nv) = reduce_cycle (v::path) w in
              visit other nv
          with Not_found -> visit (v::path) w
        ) g v;
        Hashtbl.replace visited v false
      end
    in
    G.iter_vertex (fun v -> if not (Hashtbl.mem visited v) then visit [] v) g
  ;;

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
      DotPrinter.output_graph oc pkggraph;
      close_out oc
    end 

  let load pkglist filename =
    let timer = Util.Timer.create "Defaultgraph.PackageGrah.load" in
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

module PackageGraph = MakePackageGraph(PkgV)

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

  module DotPrinter = struct
    module Display = struct
        include G
        let vertex_name uid = Printf.sprintf "\"%d\"" uid

        let graph_attributes = fun _ -> []
        let get_subgraph = fun _ -> None

        let default_edge_attributes = fun _ -> []
        let default_vertex_attributes = fun _ -> []

        let vertex_attributes v = []

        let edge_attributes e = []
      end
    include Graph.Graphviz.Dot(Display)
    let print fmt g = fprint_graph fmt g
  end 

  module DIn = Dot.Parse (Builder.I(G))(
    struct
      let node (id,_) _ =
        match id with
        |Graph.Dot_ast.String s -> int_of_string s
        |_ -> assert false
      let edge _ = ()
    end
  )

  module GmlPrinter = Gml.Print (G) (
    struct
       let node (v: G.V.label) = []
       let edge (e: G.E.label) = []
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


