  (* FIXME: 

     --> we need to be able to print the Debian original version numbers for each Cudf version of a Package
         to get interesting output: not only the version of a package existing in the repository, but also
         every version mentioned of it (create_dummy does not to this well)

     --> in clustered mode, we should only test packages in a cluster on relevant versions *for them*;
         this may require changing the logic for selecting the discriminants!

   *)

  (* FIXME: change the logic below to the following
     for all cluster
       get all versions of all packages in cluster
         -> here, skip packages with no version mentioned, and remove them from the cluster (their version change is irrelevant)
       compute discriminants overall
         foreach version
           build modified universe in lockstep
             foreach package p in cluster
               foreach q in IS(p)
                 get broken
             output info on p, precising the cluster...
   *)

  (* single package logic:
     foreach p in universe
       get all versions of p
         -> here, skip p if no version of it is mentioned
       compute discriminants
         foreach version
           build universe with dummy p with that version
             foreach q in IS(p)
               get broken
             output info on p
   *)






(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Roberto Di Cosmo <roberto@dicosmo.org>                         *)
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

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let upgradeonly = StdOpt.store_true ()
  let lockstep = StdOpt.store_true ()

  let description = "Analyse impact of version change on the impact set of packages"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'u' ~long_name:"upgradeonly" ~help:"Do not analyse version changes corresponding to downgrades" upgradeonly;
  add options ~short_name:'l' ~long_name:"lockstep" ~help:"Change version in lockstep for all packages with the same source" lockstep;
end

(* ----------------------------------- *)


let predbar = Util.Progress.create "Strongpred"
let debug fmt = Util.make_debug "Strongpred" fmt
let info fmt = Util.make_info "Strongpred" fmt
let warning fmt = Util.make_warning "Strongpred" fmt

(* collect all possible versions *)
let init_versions_table table =
  let add name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  in
  let conj_iter =
    List.iter (fun (name,sel) ->
      match sel with
      |None -> ()
      |Some(_,version) -> add name version
    )
  in
  let cnf_iter =
    List.iter (fun disjunction ->
      List.iter (fun (name,sel) ->
        match sel with
        |None -> ()
        |Some(_,version) -> add name version
      ) disjunction
    )
  in
  fun pkg ->
    add pkg.Cudf.package pkg.Cudf.version;
    conj_iter pkg.Cudf.conflicts ;
    conj_iter (pkg.Cudf.provides :> Cudf_types.vpkglist) ;
    cnf_iter pkg.Cudf.depends
;;

(* build a mapping between package names and a map old version -> new version *)
let build_table universe =
  let version_table = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (init_versions_table version_table) universe;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Hashtbl.iter (fun k {contents=l} ->
    let c = ref 1 in
    let hv = Hashtbl.create (List.length l) in
    List.iter (fun n ->
      (* Printf.eprintf "%s : %d -> %d\n" k n (2 * !c); *)
      Hashtbl.add hv n (2 * !c);
      c := !c + 1
    ) (List.sort (List.unique l));
    Hashtbl.add h k hv
  ) version_table;
  h

(* map the old universe in a new universe where all versions are even *)
let renumber universe = 
  let add table name version =
    try let l = Hashtbl.find table name in l := version::!l
    with Not_found -> Hashtbl.add table name (ref [version])
  in
  let h = build_table universe in
  let rh = Hashtbl.create (Cudf.universe_size universe) in
  let conj_map hv =
    List.map (fun (name,sel) ->
      match sel with
      |None -> (name,sel)
      |Some(c,v) -> begin
        let hv = Hashtbl.find h name in
        add rh name ((c :> Cudf_types.relop),Hashtbl.find hv v);
        (name,Some(c,Hashtbl.find hv v))
      end
    )
  in
  let cnf_map h =
    List.map (fun disjunction ->
      List.map (fun (name,sel) ->
        match sel with
        |None -> (name,sel)
        |Some(c,v) -> begin
          (* Printf.eprintf "->>>>>>>> %s %d\n" name v; *)
          let hv = Hashtbl.find h name in
          add rh name (c,Hashtbl.find hv v);
          (name,Some(c,Hashtbl.find hv v))
        end 
      ) disjunction
    )
  in
  let pkglist = 
    Cudf.fold_packages (fun acc pkg ->
      let hv = try Hashtbl.find h pkg.Cudf.package with Not_found -> assert false in
      let p = 
        { pkg with
          Cudf.version = (try Hashtbl.find hv pkg.Cudf.version with Not_found -> assert false);
          Cudf.depends = (try cnf_map h pkg.Cudf.depends with Not_found -> assert false);
          Cudf.conflicts = (try conj_map h pkg.Cudf.conflicts with Not_found -> assert false);
          Cudf.provides = (try conj_map h pkg.Cudf.provides with Not_found -> assert false)
        }
      in p::acc
    ) [] universe 
  in (rh,pkglist)

(* 
   group packages having the same source into clusters; 
   each cluster is either (p^v,[p]) if p has no source,
   or (s, pl) with pl the list of all the packages
   having source s
 *)

let src_clusters_of pkglist = 
  let add table name e =
    try let l = Hashtbl.find table name in l := e::!l
    with Not_found -> Hashtbl.add table name (ref [e])
  in
  let get_source p = Cudf.lookup_package_property p "source" in
  let h = Hashtbl.create 153 in
  List.iter (fun p ->
      let s = 
        try get_source p
        with Not_found ->
          Printf.sprintf "%s%d" p.Cudf.package p.Cudf.version
      in
      add h s p
  ) pkglist;
  Hashtbl.fold (fun k {contents=l} acc -> (k,l)::acc) h []
;;

let string_of_relop = function
  |`Eq -> "="
  |`Neq -> "!="
  |`Geq -> ">="
  |`Gt -> ">"
  |`Leq -> "<="
  |`Lt -> "<"

let string_of_package p =
  Printf.sprintf "%s" (CudfAdd.string_of_package p)
;;

let mem_package univ (p,v) =
  try ignore(Cudf.lookup_package univ (p,v)); true
  with Not_found -> false
;;

(* function to create a dummy package with a given version and name *)
let create_dummy univ p v = 
  let offset =
    if p.Cudf.version > v then 
      Printf.sprintf "[-%d]" (p.Cudf.version - v)
    else 
      Printf.sprintf "[+%d]" (v - p.Cudf.version)
  in
  let n = 
    try (Cudf.lookup_package_property p "number")^offset
    with Not_found -> Printf.sprintf "%d%s" p.Cudf.version offset
    in
  {Cudf.default_package with
   Cudf.package = p.Cudf.package;
   version = v;
   pkg_extra = [("number",`String n)] 
   }
;;
  (* discriminants takes a list of version selectors and provide a minimal list 
     of versions v1,...,vn s.t. all possible combinations of the valuse of the
     version selectors are exhibited *)
  let evalsel v = function
      (`Eq,v') -> v=v'
    | (`Geq,v') -> v>=v'
    | (`Leq,v') -> v<=v'
    | (`Gt,v') -> v>v'
    | (`Lt,v') -> v<v'
    | (`Neq,v') -> v<>v'
;;

(* perform the loop from hi to lo version, to make sure
   that = selectors are properly analysed even when 
   ignoring downgrades *)
(* for w = minv-1 to maxv+1 do *)
(* FIXME: need also to return the row associated to *any* version
   to be able, later, to avoid computing on version which have
 the same row as a version existing in the repository
*)
let discriminants sels =
  let rawvl = List.unique (List.map snd sels) in
  let minv,maxv= List.fold_left (fun (mi,ma) v -> (min mi v,max ma v)) (max_int,min_int) rawvl in
  let h = Hashtbl.create 17 in
  let h' = Hashtbl.create 17 in

  for w = maxv+1 downto 0 do
    let row = List.map (evalsel w) sels in
    if not (Hashtbl.mem h row) then begin
      Hashtbl.add h row w;
      Hashtbl.add h' w row
    end;
  done;
  Hashtbl.fold (fun k v acc -> k::acc) h' [], h'
;;

let to_set l = List.fold_right CudfAdd.Cudf_set.add l CudfAdd.Cudf_set.empty ;;

let prediction universe =
  let (version_table,pkglist) = renumber universe in
  let size = Cudf.universe_size universe in
  let res = Hashtbl.create size in
  let universe = Cudf.load_universe pkglist in
  let pkgset = to_set pkglist in

  (* usage of clusters is controlled by a command line option *)
  let clusters = 
    if (OptParse.Opt.get Options.lockstep) then
      src_clusters_of pkglist 
    else
      List.map (fun p -> 
        let s = Printf.sprintf "%s%d" p.Cudf.package p.Cudf.version in
        (s, [p])
      ) pkglist
  in

  let graph = Strongdeps.strongdeps_univ universe in

  let changed h p =
    try incr (Hashtbl.find h p) 
    with Not_found -> Hashtbl.add h p (ref 1)
  in
  Util.Progress.set_total predbar size;

  let exclude pkgset pl = 
    let sl = to_set pl in
    CudfAdd.Cudf_set.elements (CudfAdd.Cudf_set.diff pkgset sl)
  in

  List.iter
    (fun (src,cluster) ->
      Printf.printf "Analysing packages for cluster %s\n" src;
      (* collect all version constraints associated to all packages in the
       * cluster and the list of all packages in the cluster *)
      let okvl,okcl =
        List.fold_left (fun (vl,pl) p -> 
          if Hashtbl.mem version_table p.Cudf.package then
            let { contents = cl } = Hashtbl.find version_table p.Cudf.package in
            (cl@vl, p::pl)
          else begin
            (* If no version of p is explicitly dependend upon, then *)
            (* changing the version of p does not change its impact set *)
            Printf.printf
            "Skipping package %s : no version selector mentions it, so IS(p) is invariant.\n"
            (CudfAdd.string_of_package p);
            (vl,pl)
          end
        ) ([],[]) cluster
      in
      if okcl <> [] then 
        begin
          Printf.printf "Analysing cluster:\n    ";
          List.iter (fun p -> Printf.printf "%s " (string_of_package p)) okcl;
          print_newline();
          let sels = List.unique okvl in
          let vl,explain = discriminants sels in
          (* precompute impact sets of the cluster *)
          let ispl =
            let h = CudfAdd.Cudf_hashtbl.create (List.length okcl) in
            List.iter (fun p ->
              let isp = Strongdeps.impactset graph p in 
              CudfAdd.Cudf_hashtbl.add h p (List.length isp, isp)
            ) okcl;
            h
          in
          (* precompute actual versions of package in this cluster *)
          (* let clv = List.map (fun p -> p.Cudf.version, p) okcl in *)
          List.iter 
            (fun v ->
              (* compute a universe with the relevant packages in the cluster moved to version v *)
              let pl = exclude pkgset okcl in
              let okcl_at_v = 
                List.map (fun p -> 
                  if mem_package universe (p.Cudf.package,v) then p
                  else create_dummy universe p v
                ) okcl
              in
	      let at_v = 
		let h = Hashtbl.create 17 in 
		let _ = List.iter (fun p -> Hashtbl.add h p.Cudf.package p) okcl_at_v in
		fun p -> 
		  try Hashtbl.find h p.Cudf.package
		  with Not_found -> 
		    (let s = ("Nonexistent "^(string_of_package p)^" in okcl_at_v!") in
		    Printf.printf "%s : \n" s; List.iter (fun p -> Printf.printf "%s " (string_of_package p)) okcl_at_v;
		    failwith s)
	      in
              Util.Progress.progress predbar;
              let u = Cudf.load_universe (okcl_at_v@pl) in
              let s = Depsolver.load u in
              List.iter 
                (* for each package in the cluster, perform analysis *)
                (fun p -> 
                  let pn = (string_of_package p) in
                  (* FIXME: prove the following; if (p,v) and (p,w) are in U, and
                     q implies (p,v); then q is not installable when (p,w) replaces (p,v) *)
                  if p.Cudf.version = v then 
                    Printf.printf " ignoring base version %d of package %s.\n" v pn
                  else 
                    if p.Cudf.version > v && (OptParse.Opt.get Options.upgradeonly) then
                      Printf.printf " ignoring version %d of package %s : it is a downgrade\n" v pn
                    else
                      if mem_package universe (p.Cudf.package,v) then
                        Printf.printf "If we replace %s with version %d, then all its impact set becomes uninstallable.\n" pn v
                      else begin
                        let sizeisp, isp = try CudfAdd.Cudf_hashtbl.find ispl p with Not_found -> assert false in
                        begin
                          let broken =
                            List.fold_left
                              (fun acc q ->
                                (* take care of packages q in isp that may no longer be present in the updated universe *)
                                if mem_package u (q.Cudf.package,q.Cudf.version) then
                                  begin
                                    let d = Depsolver.edos_install s q in
                                    if not(Diagnostic.is_solution d) then
                                      (* record in res the changes in IS(p) for moving the cluster okcl_at_v to version v *)
                                      (changed res (p,v,okcl_at_v); q::acc) else acc
                                  end
                                else acc
                              ) [] isp
                          in
                          let nbroken = List.length broken in
                          Printf.printf " Changing version from %s to %s breaks %d/%d (=%f percent) of its Impact set.\n"
                            pn (string_of_package (at_v p)) nbroken sizeisp (float (nbroken * 100)  /. (float sizeisp));
                          Printf.printf " Version %d valuates the existing version selectors as follows:\n  " v;
                          List.iter (fun (op,v) -> Printf.printf "(%s,%d) " (string_of_relop op) v) sels; print_newline();
                          List.iter (fun v -> Printf.printf "%b " v) (List.map (evalsel v) sels); print_newline();
                          Printf.printf " The broken packages in IS(%s) are:\n" (string_of_package p);
                          List.iter (fun q -> 
                            Printf.printf "  - %s\n" (string_of_package q);
                          ) broken
                        end
                      end
                ) okcl
            ) vl
        end
    ) clusters;
  Util.Progress.reset predbar;
  res 
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug 1 ;
  let (universe,_,_) = Boilerplate.load_universe posargs in
  prediction universe
(*
  let outch = open_out "pred.table" in
  List.iter (fun (p,diff,s,d) ->
    let pkg = CudfAdd.print_package p in
    Printf.fprintf outch "%s , %d, %d, %d\n" pkg s d diff
  ) (List.sort ~cmp:(fun (_,x,_,_) (_,y,_,_) -> y - x) l);
  close_out outch
*)
;;

main();;


(* garbage collector *)

(* a function to dump the results of dsicriminants:

  Hashtbl.iter (fun v row -> Printf.printf "Version %d gives " v; List.iter (fun b -> Printf.printf "%b " b) row;print_newline()) expl;; 

Example:

# let vl,expl = discriminants [(`Eq,2);(`Eq,4)];;
val vl : int list = [4; 2; 1]
val expl : (int, bool list) Hashtbl.t = <abstr>
# Hashtbl.iter (fun v row -> Printf.printf "Version %d gives " v; List.iter (fun b -> Printf.printf "%b " b) row;print_newline()) expl;;
Version 1 gives false false 
Version 2 gives true false 
Version 4 gives false true 


*)
