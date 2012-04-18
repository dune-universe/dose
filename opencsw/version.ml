(**************************************************************************************)
(*  Copyright (C) 2009,2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                *)
(*  Copyright (C) 2009,2010 Mancoosi Project                                          *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open Common
(*
 version ::= major('.'minor('.'micro('.'qualifier)?)?)?
 major ::= digit+
 minor ::= digit+
 micro ::= digit+
 qualifier ::= (alpha|digit|'_'|'-')+
 digit ::= [0..9]
 alpha ::= [a..zA..Z]
*)

include Util.Logging(struct let label = __FILE__ end) ;;

let rex = Pcre.regexp "^\\d+(\\.\\d+(\\.\\d+(\\.[\\w_-]+)?)?)?$" ;;
let parse_version s =
  if not(Pcre.pmatch ~rex s) then 
    warning "bad version '%s'" s;
  s
;;

(*
http://www.osgi.org/javadoc/r4v42/org/osgi/framework/Version.html

A version is considered to be less than  another version if its major component
is less than the other version's major component, or the major components are
equal and its minor component is less than the other version's minor component,
or the major and minor components are equal and its micro component is less than
the other version's micro component, or the major, minor and micro components
are equal and it's qualifier component is less than the other version's
qualifier component (using String.compareTo).  A version is considered to be
equal to another version if the major, minor and micro components are equal and
the qualifier component is equal (using String.compareTo).

*)
(* for the moment we use the debian comparison function *)
let compare = Debian.Version.compare
