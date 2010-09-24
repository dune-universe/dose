(*

(* binNMU are of the for +b1 ... +bn *)
(* old binNMUs were of the form version-major.minor.binNMU *)
(** chops a possible bin-NMU suffix from a debian version string *)
let chop_binnmu s =
  let rex = Pcre.regexp "^(.* )\\+b[0-9]+$" in
  try Pcre.get_substring 1 (Pcre.exec ~rex s)
  with Not_found -> s
;;

(* *)
let chop_epoch s =
  let rex = Pcre.regexp "^[0-9]+:(.* )$" in
  try Pcre.get_substring 1 (Pcre.exec ~rex s)
  with Not_found -> s

let normalize s = chop_epoch (chop_binnmu s)

*)

(** [group_by_source universe] returns a hashtbl that maps
    (source,sourceversion) -> to a packages list *)
(* the idea is : if the normalized version of the package is equal to
 * the source version, then add it to the table indexed by source version,
 * otherwise add it to the table indexed by package version *)
(* actually it should be sourceversion -> list of list of clusters grouped by
 * version *)
let group_by_source universe =
  let th = Hashtbl.create (Cudf.universe_size universe) in
  Cudf.iter_packages (fun pkg ->
    let source = Cudf.lookup_package_property pkg "source" in
    let sourceversion = Cudf.lookup_package_property pkg "sourceversion" in
    let packageversion =  (Cudf.lookup_package_property pkg "number") in
    try
      let h = Hashtbl.find th (source,sourceversion) in
      try
        let l = Hashtbl.find h packageversion in
        l := pkg :: !l
      with Not_found -> begin (* found the source, but not the package version *)
        Hashtbl.add h packageversion (ref[pkg])
      end
    with Not_found -> begin (* didn't found the source *)
      let h = Hashtbl.create 17 in
      Hashtbl.add h packageversion (ref[pkg]);
      Hashtbl.add th (source,sourceversion) h
    end
  ) universe;
  let h = Hashtbl.create (Cudf.universe_size universe) in
  Hashtbl.iter (fun n thv ->
    let hv = Hashtbl.create 17 in
    Hashtbl.iter (fun v {contents=l} ->
      Hashtbl.add hv v l
    ) thv;
    Hashtbl.add h n hv
  ) th;
  h
