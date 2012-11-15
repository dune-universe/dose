(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

let pp_versions_table fmt (from_cudf, pkglist) =
  List.iter (fun pkg ->
    let (p,v) = from_cudf (pkg.Cudf.package,pkg.Cudf.version) in
    Format.fprintf fmt "%s=%d=%s@." p pkg.Cudf.version v
  ) pkglist

(* if at least one broken package then we set the exit code = 1 .
   we catch all other exceptions and we exit with code > 63 *)
let if_application ?(alternatives=[]) filename main =
  let open Filename in
  let normalize f = 
    try chop_extension(basename f) 
    with Invalid_argument _ -> (basename f) 
  in
  let names = List.map normalize (filename::alternatives) in
  let invoked_as = normalize Sys.argv.(0) in
  if List.exists ((=) invoked_as) names then 
    try if main () > 0 then Pervasives.exit(1)
    with exn -> fatal "%s" (Printexc.to_string exn)
  else begin
    Printf.eprintf "you are using %s as a module and not as an executable\n" Sys.argv.(0);
    Printf.eprintf "%s can be run as an exactable if named : %s\n" Sys.argv.(0) 
    (ExtString.String.join " , " names)
  end

let exit n = n
