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

let debug fmt = Util.make_debug "Debutil" fmt
let info fmt = Util.make_info "Debutil" fmt
let warning fmt = Util.make_warning "Debutil" fmt
let fatal fmt = Util.make_fatal "Debutil" fmt

(** [group_by_source universe] returns a hashtbl that maps
    (source,sourceversion) -> to a packages list *)
(* the idea is : if the normalized version of the package is equal to
 * the source version, then add it to the table indexed by source version,
 * otherwise add it to the table indexed by package version *)
(* actually it should be sourceversion -> list of list of clusters grouped by
 * version *)
let cluster packagelist =
  let drop_epoch v = let (_,v,r,b) = Version.split v in Version.concat ("",v,r,b) in
  let th = Hashtbl.create (List.length packagelist) in
  List.iter (fun pkg ->
    let packageversion = Version.normalize pkg.Packages.version in
    let realversion = drop_epoch pkg.Packages.version in
    let (source, sourceversion) =
      match pkg.Packages.source with
      |("",None) -> (pkg.Packages.name, pkg.Packages.version)
      |(n,None) -> (n, pkg.Packages.version)
      |(n,Some v) -> (n,v)
    in
    try
      let h = Hashtbl.find th (source,sourceversion) in
      try let (l,hi_v) = Hashtbl.find h packageversion in 
      l := pkg :: !l;
      let new_hi =
        if (Version.compare hi_v realversion) < 0
        then hi_v 
        else realversion
      in       
      (* keep the highest version of the cluster handy *)
      Hashtbl.replace h packageversion (l,new_hi)
      with Not_found -> 
        (* found the source, but not the package version *)
        Hashtbl.add h packageversion (ref[pkg],realversion)
    with Not_found -> begin 
      (* didn't found the source *)
      let h = Hashtbl.create 17 in
      Hashtbl.add h packageversion (ref[pkg],realversion);
      Hashtbl.add th (source,sourceversion) h
    end
  ) packagelist ;
  let h = Hashtbl.create (List.length packagelist) in
  let i = ref 0 in
  Hashtbl.iter (fun (s,v) thv ->
    let l = Hashtbl.fold (fun v ({contents=l},rv) acc -> (v,rv,l)::acc) thv [] in
    i := !i + (List.length l);
    Hashtbl.add h (s,v) l
  ) th;
  info "Packages: %d" (List.length packagelist);
  info "Source Clusters: %d" (Hashtbl.length h);
  info "Binary (effective) Clusters: %d" !i;
  h
;;
