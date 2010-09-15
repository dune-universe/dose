
(** [group_by_source universe] returns a hashtbl that maps
    (source,sourceversion) -> to a reference of a packages list *)
let group_by_source universe =
  let th = Hashtbl.create (Cudf.universe_size universe) in
  let add h k v =
     try let l = Hashtbl.find h k in l := v::!l
     with Not_found -> Hashtbl.add h k (ref [v])
  in
  Cudf.iter_packages (fun pkg ->
    let source = Cudf.lookup_package_property pkg "source" in
    let sourceversion = Cudf.lookup_package_property pkg "sourceversion" in
    add th (source,sourceversion) pkg
  ) universe;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Hashtbl.iter (fun k {contents=l} -> Hashtbl.add h k l) th;
  h
