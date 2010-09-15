(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):                                                       *)
(*    Copyright (C) 2009,2010 Pietro Abate <pietro.abate@pps.jussieu.fr>  *)
(*                                                                        *)
(*  Contributor(s):                                                       *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

val debug : ('a, unit, string, unit) format4 -> 'a
val info : ('a, unit, string, unit) format4 -> 'a
val warning : ('a, unit, string, unit) format4 -> 'a

open ExtLib

type conversion

(** [constraints universe] returns a map between package names
    and an ordered list of constraints where the package name is
    mentioned *)
val constraints :
  Cudf.universe ->
  (Cudf_types.pkgname, (Cudf_types.relop * Cudf_types.version) list)
  Hashtbl.t

(* map the old universe in a new universe where all versions are even
   the from_cudf function returns real versions for even cudf ones   
   and a representation of the interval n-1, n+1 for odd cudf ones    *)
val renumber :
  Cudf.universe * (string * int -> string) * (string * string -> int) ->
  conversion

(** create a dummy package with a given version and name and an extra property
    'number' with a representation of version v *)
val create_dummy :
  conversion -> Cudf_types.pkgname * Cudf_types.version -> Cudf.package

(* discriminants takes a list of version selectors and provide a hashtbl with
   keys the minimal list of versions v1,...,vn s.t. all possible combinations 
   of the valuse of the version selectors are exhibited, and values the
   evaluation list asosciated to the version *)
val discriminants :
  ?vl:int list ->
  (Cudf_types.relop * Cudf_types.version) list ->
  (int, bool list) Hashtbl.t

(** [migrate table v l] migrates all packages in [l] to version [v] *)
val migrate :
  conversion -> Cudf_types.version -> Cudf.package list -> Cudf.package list

