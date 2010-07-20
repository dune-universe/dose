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

open Common
open CudfAdd

(** strongconflicts return the list of all strong conflicts in universe.
    
    invariant: the universe must contain only edos-installable packages : see
    Depsolver.trim.
*)
let strongconflicts universe =
  let mdf = Mdf.load_from_universe (Depsolver.trim universe) in
  let maps = mdf.Mdf.maps in
  (* let idlist = Cudf.fold_packages (fun l p -> (maps.map#vartoint p)::l) [] (Depsolver.trim universe) in *)
  let l = Strongconflicts_int.strongconflicts mdf (* idlist *) in
  List.map (fun (x,y) -> (maps.map#inttovar x,maps.map#inttovar y)) l

