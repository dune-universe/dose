
(** Load default modules defined in Ipr *)

module Cudf = Ipr.Cudf
module Cudf_printer = Ipr.Cudf_printer
module Cudf_parser = Ipr.Cudf_parser
module Cudf_checker = Ipr.Cudf_checker

(* I want to hash packages by name/version without considering
   other fields like Installed / keep / etc.
*)
module Cudf_hashtbl = 
  Hashtbl.Make(struct 
    type t = Cudf.package
    let equal = Cudf.(=%)
    let hash pkg = Hashtbl.hash (pkg.Cudf.package, pkg.Cudf.version)
  end)

let parse_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    Cudf_parser.load p
  with
    Cudf_parser.Parse_error _
    | Cudf.Constraint_violation _ as exn ->
      Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
      doc (Printexc.to_string exn);
      exit 1

