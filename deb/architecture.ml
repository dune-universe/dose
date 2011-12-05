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

(*
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

*)

open String;;

let src_matches_arch source concrete =
  let component_matches source concrete = (source="any" || source=concrete)
  in
  if source="all" || component_matches source concrete then true
  else
    (* the source architecture must consist of two parts: os and cpu *)
    try
      let i=index source '-' in
      let source_os=sub source 0 i
      and source_cpu= sub source (i+1) ((length source)-i-1) in
      (* If the concrete architecture also splits into OS and Host
	 then components must match. Otherwise we have an implicit
	 OS which is linux *)
      try
	let j=index concrete '-' in
	let concrete_os=sub concrete 0 j
	and concrete_cpu=sub concrete (j+1) ((length concrete)-j-1)
	in (component_matches source_os concrete_os) &&
	  (component_matches source_cpu concrete_cpu)
      with Not_found -> 
	(source_os="linux" || source_os="any") &&
	  component_matches source_cpu concrete
    with Not_found -> false
;;
      
