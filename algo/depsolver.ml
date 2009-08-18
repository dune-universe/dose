
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

let edos_install s request =
  Depsolver_int.edos_install (s.solver,s.maps) request

let edos_coinstall s request_lst =
  Depsolver_int.edos_coinstall (s.solver,s.maps) request_lst

let dependency_closure universe l =
  let maps = Depsolver_int.build_maps universe in
  Depsolver_int.dependency_closure maps l

let dump s = Depsolver_int.S.dump s.solver.constraints
