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

open ExtLib
open Cudf
open Common
open Depsolver_int

type solver = Depsolver_int.solver

let init ?(buffer=false) cudf_universe =
  let (solver,maps) = Depsolver_int.init buffer cudf_universe in
  { universe = cudf_universe ;
    maps = maps ;
    solver = solver 
  } 

let solve s request =
  Depsolver_int.solve (s.solver,s.maps) request

let distribcheck ?callback s =
  Depsolver_int.distribcheck ?callback (s.solver,s.maps) s.universe

let pkglistcheck ?callback s pkglist =
  Depsolver_int.pkglistcheck ?callback (s.solver,s.maps) pkglist

let edos_install s request =
  Depsolver_int.edos_install (s.solver,s.maps) request

let edos_coinstall s request_lst =
  Depsolver_int.edos_coinstall (s.solver,s.maps) request_lst

let dependency_closure universe l =
  let maps = Depsolver_int.build_maps universe in
  Depsolver_int.dependency_closure maps l

let dump s = Depsolver_int.S.dump s.solver.constraints
