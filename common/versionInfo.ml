
let svn_rev = "$Rev$"
let svn_lastchangedate = "$Date$"
let ocaml_version = Sys.ocaml_version
let build_timestamp = string_of_float (Unix.time ())
let build_host = Unix.gethostname ()
let build_os = Sys.os_type
(* let buidl_user = Unix.getlogin () *)
let version = "Build version"
