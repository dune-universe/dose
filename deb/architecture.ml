(***************************************************************************)
(*  Copyright (C) 2010 Ralf Treinen <ralf.treinen@pps.jussieu.fr>          *)
(*                                                                         *)
(*  This library is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU Lesser General Public License as         *)
(*  published by the Free Software Foundation, either version 3 of the     *)
(*  License, or (at your option) any later version.  A special linking     *)
(*  exception to the GNU Lesser General Public License applies to this     *)
(*  library, see the COPYING file for more information.                    *)
(***************************************************************************)

exception Architectures_inconsistent;;

let bin_unify s1 s2 =
  if s1=s2 || s1="all"
  then s2
  else if s2="all" then s1 else raise Architectures_inconsistent
;;

(bin_unify "all" "amd64");;
(bin_unify "amd64" "all");;
(bin_unify "all" "all");;
(bin_unify "amd64" "i386");;

(* Debian Policy Manual version 3.9.1.0, 2010-07-26 :                      *)
(* Section 11.1.1 Architecture wildcards                                   *)
(* A package may specify an architecture wildcard. Architecture wildcards  *)
(* are in the format any (which matches every architecture), os-any,       *)
(* or any-cpu.                                                             *)

open String;;

let bin_matches_src b s =
  let part_matches b s = (s="any" || s=b)
  in
  if s="all" || s="any" || s=b then true
  else
    try
      let i=index s '-' in
      let sos=sub s 0 i
      and scpu= sub s (i+1) ((length s)-i-1) in
      try
	let j=index b '-' in
	let bos=sub b 0 j
	and bcpu=sub b (j+1) ((length b)-j-1)
	in (part_matches bos sos) && (part_matches bcpu scpu)
      with Not_found -> (sos="linux" || sos="any") && part_matches b scpu
    with Not_found -> false
;;
      
