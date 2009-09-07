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

open Graph
open Common

let print_package ?(short=true) pkg =
  if short then
    let (sp,sv) =
      try (pkg.Cudf.package,Cudf.lookup_package_property pkg "Number")
      with Not_found -> (pkg.Cudf.package,string_of_int pkg.Cudf.version)
    in Printf.sprintf "%s (= %s)" sp sv
  else
    Cudf_printer.string_of_package pkg

module SyntacticDependencyGraph = struct

  module PkgV = struct
      type t = Pkg of Cudf.package | Or of (Cudf.package * int)
      (* XXX ok, att here. if I just use Pervasive.compare the records
       * representing the same Cudf package (that is Cudf.(=%) x y = true)
       * will result structully different . Do we loose in performance here ? *)
      let compare x y =
        match (x,y) with
        |Pkg x , Pkg y -> if Cudf.(=%) x y then 0 else -1
        |Or(x,i),Or(y,j) -> if (i = j) && Cudf.(=%) x y then 0 else -1
        |_ -> 1
      let hash p =
        match p with
        |Pkg p -> Hashtbl.hash (p.Cudf.package,p.Cudf.version)
        |Or (p,i) -> Hashtbl.hash (p.Cudf.package,p.Cudf.version,i)
      let equal x y =
        match (x,y) with
        |Pkg x , Pkg y -> Cudf.(=%) x y
        |Or(x,i),Or(y,j) -> (i = j) && (Cudf.(=%) x y)
        |_ -> false
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
        |PkgV.Pkg i -> Printf.sprintf "\"%s\"" (print_package i)
        |PkgV.Or (i,c) -> Printf.sprintf "\"Or%s-%d\"" (print_package i) c

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

module BidirectionalGraph = struct

  (* XXX ok, att here. if I just use Pervasive.compare the records
   * representing the same Cudf package (that is Cudf.(=%) x y = true)
   * will result structully different . Do we loose in performance here ? *)
  module PkgV = struct
      type t = Cudf.package
      let compare x y = if Cudf.(=%) x y then 0 else -1
      let hash p = Hashtbl.hash (p.Cudf.package,p.Cudf.version)
      let equal = Cudf.(=%)
  end

  module G = Imperative.Digraph.ConcreteBidirectional(PkgV)

  module Display =
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (print_package v)

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
