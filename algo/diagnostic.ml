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

type reason =
  |Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
  |EmptyDependency of (Cudf.package * Cudf_types.vpkg list)
  |Conflict of (Cudf.package * Cudf.package)

type request =
  |Package of Cudf.package
  |PackageList of Cudf.package list

type result =
  |Success of (unit -> Cudf.package list)
  |Failure of (unit -> reason list)

type diagnosis = { result : result ; request : request }

let print_package = CudfAdd.print_package

let print_request ?(pp=CudfAdd.print_package) = function
  |Package p -> (pp ~short:true p)
  |PackageList pl -> String.concat "," (List.map (pp ~short:true) pl)

let is_solution = function
  |{result = Success _ } -> true
  |{result = Failure _ } -> false

let print ?(pp=CudfAdd.print_package) ?(explain=false) oc result =
  match result,explain with
  |{ result = Failure (_) ; request = r },false ->
      Printf.fprintf oc "%s: FAILED\n" (print_request ~pp r)
  |{ result = Success (_); request = r },false ->
      Printf.fprintf oc "%s: SUCCESS\n" (print_request ~pp r)
  |{ result = Success (f); request = r },true ->
      begin
        (* Printf.fprintf oc "%s: SUCCESS\n" (print_request ~pp r) ; *)
        List.iter (fun p ->
          Printf.fprintf oc "%s\n" (pp ~short:false p)
        ) (f ())
      end
  |{result = Failure (f) ; request = r },true -> 
     begin
       Printf.fprintf oc "%s: FAILED\n" (print_request ~pp r) ;
       List.iter (function
         |Dependency(i,vpkgs,l) ->
            let l = List.map (pp ~short:true) l in
            Printf.fprintf oc
            "Dependency Problem. Package %s has an unfulfilled dependency on\n %s\n Expanded as %s\n\n"
            (pp ~short:true i)
            (String.concat " | " (
              List.map (fun vpkg -> (Cudf_types_pp.string_of_vpkg vpkg)) vpkgs)
            )
            (String.concat " , " l)
         |Conflict (i,j) ->
            Printf.fprintf oc
            "There is a Conflict between package %s and package %s\n"
            (pp ~short:true i) (pp ~short:true j)
         |EmptyDependency (i,vpkgs) ->
            Printf.fprintf oc
            "Package %s has dependencies that cannot be satisfied: %s\n"
            (pp ~short:true i) 
            (String.concat " | " (
              List.map (fun vpkg -> (Cudf_types_pp.string_of_vpkg vpkg)) vpkgs)
            )
       ) (f ())
   end
