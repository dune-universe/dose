
open Graph
open Common

module SyntacticDependencyGraph(Pr : sig val pr : Cudf.package -> string end) = struct

  module PkgV = struct
      type t = Pkg of Cudf.package | Or of (Cudf.package * int)
      let compare = Pervasives.compare
      let hash = Hashtbl.hash
      let equal l1 l2 = (l1 = l2)
  end

  module PkgE = struct
    type t = OrDepends | DirDepends | Conflict

    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal l1 l2 = (l1 = l2)
    let default = DirDepends
  end

  module G = Imperative.Digraph.ConcreteLabeled(PkgV)(PkgE) 

  module Display = struct
      include G
      let vertex_name v =
        match G.V.label v with
        |PkgV.Pkg i -> Printf.sprintf "\"%s\"" (Pr.pr i)
        |PkgV.Or (i,c) -> Printf.sprintf "\"Or%s-%d\"" (Pr.pr i) c

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v =
        match G.V.label v with
        |PkgV.Or _ -> [`Label "Or"]
        |_ -> []

      let edge_attributes e =
        let t =
          match G.E.label e with
          |PkgE.DirDepends -> [`Style `Dotted]
          |PkgE.OrDepends -> [`Style `Dotted]
          |PkgE.Conflict -> [`Style `Dotted; `Label "#"]
        in
        t
    end

  module D = Graph.Graphviz.Dot(Display) 
  module S = Set.Make(PkgV)

  let dependency_graph universe =
    let maps = CudfAdd.build_maps universe in
    let gr = G.create () in
    Cudf.iter_packages (fun pkg ->
      let vpid = G.V.create (PkgV.Pkg pkg) in
      let c = ref 0 in
      G.add_vertex gr vpid ;
      List.iter (function
        |[(pkgname,constr)] ->
            List.iter (fun p ->
              let vp = G.V.create (PkgV.Pkg p) in
              let edge = G.E.create vpid PkgE.DirDepends vp in
              G.add_vertex gr vp ;
              G.add_edge_e gr edge
            ) (maps.CudfAdd.who_provides (pkgname,constr))
        |l -> begin
            let vor = G.V.create (PkgV.Or (pkg,!c)) in
            G.add_vertex gr vor;
            let edgeor = G.E.create vpid PkgE.OrDepends vor in
            G.add_edge_e gr edgeor;
            incr c;
            List.iter (fun (pkgname,constr) ->
              List.iter (fun p ->
                let vp = G.V.create (PkgV.Pkg p) in
                G.add_vertex gr vp;
                let oredge = G.E.create vor PkgE.OrDepends vp in
                G.add_edge_e gr oredge
              ) (maps.CudfAdd.who_provides (pkgname,constr))
            ) l
        end
      ) pkg.Cudf.depends
      ;
      List.iter (fun (pkgname,constr) ->
        List.iter (fun p ->
          let vp = G.V.create (PkgV.Pkg p) in
          let edge = G.E.create vpid PkgE.Conflict vp in
          G.add_vertex gr vp;
          G.add_edge_e gr edge
        ) (maps.CudfAdd.who_provides (pkgname,constr))
      ) pkg.Cudf.conflicts
    ) universe
    ;
    gr
  ;;

end

(******************************************************)

module BidirectionalGraph(Pr : sig val pr : Cudf.package -> string end) = struct

  module PkgV = struct
      type t = Cudf.package
      let compare = Pervasives.compare
      let hash = Hashtbl.hash
      let equal l1 l2 = (l1 = l2)
  end

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)

  module Display =
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (Pr.pr v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v = []

      let edge_attributes e = []
    end
  
  module D = Graph.Graphviz.Dot(Display)
  module S = Set.Make(PkgV)
end

(******************************************************)

module MatrixGraph(Pr : sig val pr : int -> string end) = struct

  module G = Imperative.Matrix.Digraph

  module Display =
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (Pr.pr v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v = []

      let edge_attributes e = []
    end
  
  module D = Graph.Graphviz.Dot(Display)
  module S = Set.Make(struct type t = int let compare = compare end)
end
