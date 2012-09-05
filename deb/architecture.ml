(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2010,2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

let src_matches_arch source host =
  (* matching of an OS or CPU component *)
  let component_matches source host = (source="any" || source=host)
  (* split an architecture string into OS and CPU, linux is default when
     no OS is given. *)
  and split arch =
    try
      let dash=String.index arch '-' in
      (String.sub arch 0 dash,
       String.sub arch (dash+1) ((String.length arch)-dash-1))
    with
	Not_found -> ("linux",arch)
  in
  if source="all" || source="any"  then true
  else
    let source_os,source_cpu = split source
    and host_os,host_cpu = split host in
    (component_matches source_os host_os) &&
      (component_matches source_cpu host_cpu)
;;
