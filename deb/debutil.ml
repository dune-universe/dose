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

(** [group_by_source universe] returns a hashtbl that maps
    (source,sourceversion) -> to a packages list *)
(* the idea is : if the normalized version of the package is equal to
 * the source version, then add it to the table indexed by source version,
 * otherwise add it to the table indexed by package version *)
(* actually it should be sourceversion -> list of list of clusters grouped by
 * version *)
let cluster packagelist =
  let th = Hashtbl.create (List.length packagelist) in
  List.iter (fun pkg ->
    let packageversion = Version.normalize pkg.Packages.version in
    let (source, sourceversion) =
      match pkg.Packages.source with
      |("",None) -> (pkg.Packages.name, packageversion)
      |(n,None) -> (n, packageversion)
      |(n,Some v) -> (n,v)
    in
    try
      let h = Hashtbl.find th (source,sourceversion) in
      try let l = Hashtbl.find h packageversion in l := pkg :: !l
      with Not_found -> 
        (* found the source, but not the package version *)
        Hashtbl.add h packageversion (ref[pkg])
    with Not_found -> begin 
      (* didn't found the source *)
      let h = Hashtbl.create 17 in
      Hashtbl.add h packageversion (ref[pkg]);
      Hashtbl.add th (source,sourceversion) h
    end
  ) packagelist ;
  let h = Hashtbl.create (List.length packagelist) in
  Hashtbl.iter (fun (s,v) thv ->
    let l = Hashtbl.fold (fun v {contents=l} acc -> (v,l)::acc) thv [] in
    Hashtbl.add h (s,v) l
  ) th;
  h
;;
