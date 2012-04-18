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
  let table =  StdOpt.store_true ()
  let detrans = StdOpt.store_true ()
  let trans_closure = StdOpt.store_true ()
  let checkonly = Boilerplate.vpkglist_option ()
  let prefix = StdOpt.str_option ~default:"" ()
  let conj_only = StdOpt.store_true ()

  open OptParser
  add options ~long_name:"prefix" ~help:"Prefix output fils with <prefix>" prefix;
  add options ~long_name:"dot" ~help:"Print the strong dependency graph in dot format" dot;
  add options ~long_name:"table" ~help:"Print the table (package,strong,direct,difference)" table;
  add options ~long_name:"detrans" ~help:"Perform the transitive reduction of the strong dependency graph" detrans;
  add options ~long_name:"transitive-closure" ~help:"Perform the transitive closure of the direct dependency graph" trans_closure;
  add options ~long_name:"checkonly" ~help:"Check only these package" checkonly;
  add options ~long_name:"conj-only" ~help:"Use the conjunctive graph only" conj_only;
end

include Util.Logging(struct let label = __FILE__ end) ;;

module G = Defaultgraphs.PackageGraph.G
module O = Defaultgraphs.GraphOper(G)

(* ----------------------------------- *)

let impactlist = Defaultgraphs.PackageGraph.pred_list
let rev_impactlist = Defaultgraphs.PackageGraph.succ_list

let mk_filename prefix suffix s = if prefix = "" then s^suffix else prefix^suffix

let default_options = function
  |Url.Deb -> Some ( 
    Boilerplate.Deb { 
      Debian.Debcudf.default_options with
      Debian.Debcudf.ignore_essential = true
    })
  |Url.Synthesis -> None
  |Url.Hdlist -> None
  |(Url.Pgsql|Url.Sqlite) -> None
  |Url.Eclipse -> Some (Boilerplate.Eclipse Debian.Debcudf.default_options)
  |Url.Cudf -> None
  |Url.Csw -> Some (Boilerplate.Csw Debian.Debcudf.default_options)
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = ["Strongdeps_int.main";"Strongdeps_int.conj"] in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) bars;
  let options = default_options (Input.guess_format [posargs]) in
  let (_,universe,_,to_cudf) = Boilerplate.load_universe ~options posargs in
  let prefix = OptParse.Opt.get Options.prefix in
  if OptParse.Opt.is_set Options.checkonly then begin
    let pkglistlist =
      List.map (function
        |(p,None) -> Cudf.lookup_packages universe p
        |(p,Some(c,v)) ->
            let filter = Some(c,snd(to_cudf (p,v))) in
            Cudf.lookup_packages ~filter universe p
      ) (OptParse.Opt.get Options.checkonly)
    in
    List.iter (fun pkglist ->
      (* if --checkonly we compute the strong dependencies of the pkglist and
       * the we print either the detrans graph or a simple yaml list of packages *)
      let sdgraph =
        if OptParse.Opt.get Options.conj_only then 
          Strongdeps.conjdeps universe pkglist
        else 
          Strongdeps.strongdeps universe pkglist
      in
      if OptParse.Opt.get Options.dot then begin
        Defaultgraphs.PackageGraph.D.output_graph stdout sdgraph;
      end else begin
        let pp_list = Diagnostic.pp_list CudfAdd.pp_package in
        List.iter (fun q -> 
          let l = rev_impactlist sdgraph q in
          Format.printf "@[<v 1>root: %s@," (CudfAdd.string_of_package q);
          if List.length l > 0 then
            Format.printf "@[<v 1>strongdeps:@,%a@]" pp_list l
          else
            Format.printf "@[<v 1>strongdeps: no direct strong dependencies@]";
          Format.printf "@]@."
        ) pkglist
      end
    ) pkglistlist
  end else begin
    let sdgraph = 
      if OptParse.Opt.get Options.conj_only then
        Strongdeps.conjdeps_univ universe
      else 
        Strongdeps.strongdeps_univ universe
    in

    if OptParse.Opt.get Options.detrans then
      O.transitive_reduction sdgraph;

    let outch = 
      if OptParse.Opt.get Options.table then
        open_out (mk_filename prefix ".csv" "data")
      else 
        stdout
    in
    let depgraph =
      if OptParse.Opt.get Options.trans_closure then
        O.O.transitive_closure (Defaultgraphs.PackageGraph.dependency_graph universe)
      else
        Defaultgraphs.PackageGraph.dependency_graph universe 
    in
    let l = 
      G.fold_vertex (fun p l ->
        let uid = p in
        let strongimpact = List.length (impactlist sdgraph uid) in 
        let rev_strongimpact = List.length (rev_impactlist sdgraph uid) in
        let directimpact = List.length (impactlist depgraph p) in
        let rev_directimpact = List.length (rev_impactlist depgraph p) in
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
    ;
    let dot = 
      if OptParse.Opt.get Options.dot then 
        Some (mk_filename prefix ".dot" "graph") 
      else None 
    in
    if (OptParse.Opt.get Options.dot) then
      let pkggraph = sdgraph in
      Defaultgraphs.PackageGraph.out ~dot pkggraph
  end
;;

main ();;
