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

open String;;

(* Debian Policy Manual version 3.9.1.0, 2010-07-26 :                      *)
(* Section 11.1.1 Architecture wildcards                                   *)
(* A package may specify an architecture wildcard. Architecture wildcards  *)
(* are in the format any (which matches every architecture), os-any,       *)
(* or any-cpu.                                                             *)

type arch_os = OS_Linux | OS_Hurd | OS_Kfreebsd;;
type arch_cpu = string;;
type architecture =
    DAall
  | DAany
  | DAanyos of arch_cpu
  | DAanycpu of arch_os
  | DApair of arch_os * arch_cpu
;;

let os_of_string s =
  if s="linux" then OS_Linux
  else if s="kfreebsd" then OS_Kfreebsd
  else if s="hurd" then OS_Hurd
  else failwith ("Unknown OS specififier:"^s)
;;

let string_of_os = function
    OS_Linux -> "linux"
  | OS_Hurd -> "hurd"
  | OS_Kfreebsd -> "kfreebsd"
;;

let architecture_of_string s =
  if s="all" then DAall
  else if s="any" then DAany
  else 
    try
      let i=index s '-'
      in let os=sub s 0 i
	 and cpu= sub s (i+1) ((length s)-i-1)
      in
	if os="any"
	then DAanyos(cpu)
	else if cpu="any"
	then DAanycpu(os_of_string os)
	else DApair(os_of_string os, cpu)
    with Not_found -> DApair(OS_Linux,s)
;;  

let string_of_architecture = function
    DAall -> "all"
  | DAany -> "any"
  | DAanyos(cpu) -> "any-"^cpu
  | DAanycpu(os) -> (string_of_os os)^"-any"
  | DApair(OS_Linux,cpu) -> cpu
  | DApair(os,cpu) -> (string_of_os os)^"-"^cpu
;;

exception Arch_unification_error of string;;

let error(al,ar) = raise
  (Arch_unification_error
     ("Architectures "^(string_of_architecture al)^" and "^
	(string_of_architecture ar)^" are not compatible"))
;;

let arch_unify al ar = match al with
  | DAall | DAany -> ar
  | DAanyos(cpul) -> 
      begin
	match ar with
	  | DAall | DAany -> al
	  | DAanyos(cpur) -> if cpul=cpur then al else error(al,ar)
	  | DAanycpu(osr) -> DApair(osr,cpul)
	  | DApair(osr,cpur) -> if cpul=cpur then ar else error(al,ar)
      end
  | DAanycpu(osl) ->
      begin
	match ar with
	  | DAall | DAany -> al
	  | DAanyos(cpur) -> DApair(osl,cpur)
	  | DAanycpu(osr) -> if osl=osr then al else error(al,ar)
	  | DApair(osr,cpur) -> if osl=osr then ar else error(al,ar)
      end
  | DApair(osl,cpul) ->
      begin
	match ar with
	  | DAall | DAany -> al
	  | DAanyos(cpur) -> if cpul=cpur then al else error(al,ar)
	  | DAanycpu(osr) -> if osl=osr then al else error(al,ar)
	  | DApair(osr,cpur) -> if osl=osr && cpul=cpur then al else error(al,ar)
      end
    
    

