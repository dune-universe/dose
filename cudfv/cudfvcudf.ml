

open ExtLib
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

type tables = {
  reverse_table : ((string * int), string) Hashtbl.t; (** (name,cudf_version) -> real_version  *)
  version_table : ((string * string), int) Hashtbl.t; (** (name,real_version) -> cudf_version  *)
}

let create n = {
  reverse_table = Hashtbl.create (10 * n);
  version_table = Hashtbl.create (10 * n);
}

type options = {
  rvf : string;
  cudfv: bool;
}

let default_options = {
  rvf = "number";
  cudfv = false;
}

let add table k v = 
  if not (Hashtbl.mem table k) then
    Hashtbl.add table k v
  else ()

let add_pkg tables field pkg =
  let name = CudfAdd.get_property "package" pkg in
  let cudf_version = int_of_string (CudfAdd.get_property "version" pkg) in
  let version =
    try
    CudfAdd.get_property field pkg
    with Not_found -> 
      warning "Package (%s,%d) does not have a \"%s\" field " name cudf_version field;
      string_of_int cudf_version
  in
  add tables.reverse_table (name,cudf_version) version;
  add tables.version_table (name,version) cudf_version

let add_v2v tables name cudf_version version =
  let cudf_name = CudfAdd.encode name in
  add tables.reverse_table (cudf_name,cudf_version) version;
  add tables.version_table (cudf_name,version) cudf_version  

let parse_v2v tables filename = 
  let ic = open_in filename in
      try
	while true do
	  let line = input_line ic in
	  try
	    Scanf.sscanf line "#v2v:%s@:%d=%s" (add_v2v tables)
	  with Scanf.Scan_failure _ | End_of_file -> ()
	done
      with End_of_file ->
	close_in ic

let init_tables options pkglist filename  =
  let size = List.length pkglist in
  let tables = create size in
  parse_v2v tables filename;
  List.iter (add_pkg tables options.rvf) pkglist; 
  tables

let get_real_version tables (name,cudf_version)  =
  try Hashtbl.find tables.reverse_table (name,cudf_version)
  with Not_found ->
    warning "Package (%s,%d) does not have an associated real version" name cudf_version;
    string_of_int cudf_version

let get_cudf_version tables (name,real_version) = 
  try Hashtbl.find tables.version_table (name,real_version)
  with Not_found -> begin
    warning "Package (%s,%s) does not have an associated cudf version" name real_version;
    raise Not_found
  end

let tocudf pkg = pkg

let preamble = Cudf.default_preamble
