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

open ExtLib
open Common
open Algo

module Options = struct
  open OptParse
  let description = "Compute the strong dependency graph"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let dot = StdOpt.store_true ()
  let dump = StdOpt.store_true ()
  let table =  StdOpt.store_true ()
  let detrans = StdOpt.store_true ()
  let trans_closure = StdOpt.store_true ()
  let checkonly = Boilerplate.vpkglist_option ()
  let prefix = StdOpt.str_option ~default:"" ()
  let conj_only = StdOpt.store_true ()

  open OptParser
  add options ~long_name:"prefix" ~help:"Prefix output fils with <prefix>" prefix;
  add options ~long_name:"dot" ~help:"Save the strong dependency graph in dot format" dot;
  add options ~long_name:"dump" ~help:"Save the strong dependency graph" dump;
  add options ~long_name:"table" ~help:"Print the table (package,strong,direct,difference)" table;
  add options ~long_name:"detrans" ~help:"Perform the transitive reduction of the strong dependency graph" detrans;
  add options ~long_name:"transitive-closure" ~help:"Perform the transitive closure of the direct dependency graph" trans_closure;
  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;
  add options ~long_name:"conj-only" ~help:"Use the conjunctive graph only" conj_only;
end

module G = Defaultgraphs.IntPkgGraph.G
module O = Defaultgraphs.GraphOper(G)

(* ----------------------------------- *)

let impactlist graph q =
  Defaultgraphs.IntPkgGraph.G.fold_pred (fun p acc -> p :: acc ) graph q []

let rev_impactlist graph q =
  Defaultgraphs.IntPkgGraph.G.fold_succ (fun p acc -> p :: acc ) graph q []

let impactlist_p graph q =
  Defaultgraphs.PackageGraph.G.fold_pred (fun p acc -> p :: acc ) graph q []

let rev_impactlist_p graph q =
  Defaultgraphs.PackageGraph.G.fold_succ (fun p acc -> p :: acc ) graph q []


let mk_filename prefix suffix s = if prefix = "" then s^suffix else prefix^suffix

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Strongdeps_int.main";"Strongdeps_int.conj"] in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) bars;
  let (_,universe,_,to_cudf) = Boilerplate.load_universe posargs in
  let prefix = OptParse.Opt.get Options.prefix in
  let sdgraph = 
    if OptParse.Opt.is_set Options.checkonly then
      let pkglist =
        List.flatten (
          List.map (function
            |(p,None) -> Cudf.lookup_packages universe p
            |(p,Some(c,v)) ->
                let filter = Some(c,snd(to_cudf (p,v))) in
                Cudf.lookup_packages ~filter universe p
          ) (OptParse.Opt.get Options.checkonly)
        )
      in
      if OptParse.Opt.get Options.conj_only then 
        Strongdeps.conjdeps universe pkglist
      else 
        Strongdeps.strongdeps universe pkglist
    else
      if OptParse.Opt.get Options.conj_only then
        Strongdeps.conjdeps_univ universe
      else 
        Strongdeps.strongdeps_univ universe
  in
  if OptParse.Opt.get Options.detrans then
    O.transitive_reduction sdgraph;
  if OptParse.Opt.is_set Options.checkonly then begin
    let pkggraph = Defaultgraphs.intcudf universe sdgraph in
    Defaultgraphs.PackageGraph.D.output_graph stdout pkggraph;
  end;
  if not(OptParse.Opt.is_set Options.checkonly) then begin
    let outch = 
      if OptParse.Opt.get Options.table then
        open_out (mk_filename prefix ".csv" "data")
      else 
        stdout
    in
    let depgraph =
      let module O = Defaultgraphs.GraphOper(Defaultgraphs.PackageGraph.G) in
      if OptParse.Opt.get Options.trans_closure then
        O.O.transitive_closure (Defaultgraphs.PackageGraph.dependency_graph universe)
      else
        Defaultgraphs.PackageGraph.dependency_graph universe 
    in
    let l = 
      Defaultgraphs.PackageGraph.G.fold_vertex (fun p l ->
        let uid = Cudf.uid_by_package universe p in
        let strongimpact = List.length (impactlist sdgraph uid) in 
        let rev_strongimpact = List.length (rev_impactlist sdgraph uid) in
        let directimpact = List.length (impactlist_p depgraph p) in
        let rev_directimpact = List.length (rev_impactlist_p depgraph p) in
        (p,strongimpact - directimpact, 
          rev_strongimpact, strongimpact, 
          rev_directimpact, directimpact) :: l
      ) depgraph []
    in
    Printf.fprintf outch "name, #str-out, #str-in, #dir-out, #dir-in, diff\n";
    List.iter (fun (p,diff,rs,s,rd,d) ->
      let pkg = CudfAdd.string_of_package p in
      Printf.fprintf outch "%s , %d, %d, %d, %d, %d\n" pkg rs s rd d diff
    ) (List.sort ~cmp:(fun (_,x,_,_,_,_) (_,y,_,_,_,_) -> y - x) l);
    close_out outch
  end ;
  let dump = 
    if OptParse.Opt.get Options.dump then 
      Some (mk_filename prefix ".dump" "data") 
    else None 
  in
  let dot = 
    if OptParse.Opt.get Options.dot then 
      Some (mk_filename prefix ".dot" "graph") 
    else None 
  in
  if (OptParse.Opt.get Options.dump) || (OptParse.Opt.get Options.dot) then
    let pkggraph = Defaultgraphs.intcudf universe sdgraph in
    Defaultgraphs.PackageGraph.out 
      ~dump ~dot ~detrans:(OptParse.Opt.get Options.detrans) pkggraph
;;

main ();;
