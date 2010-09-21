
(* binNMU are of the for +b1 ... +bn *)
(* old binNMUs were of the form version-major.minor.binNMU *)
(** chops a possible bin-NMU suffix from a debian version string *)
let chop_binnmu s =
  try
    Str.string_before s
      (Str.search_backward (Str.regexp "\\+b[0-9]+$") s ((String.length s)-1))
  with
      Not_found -> s
;;

(* *)
let chop_epoch s =
  (* let epoch_re = Pcre.regexp "^[0-9]+:(.* )$" in *) s

let normalize s = chop_epoch (chop_binnmu s)

(** [group_by_source universe] returns a hashtbl that maps
    (source,sourceversion) -> to a packages list *)
let group_by_source universe =
  let th = Hashtbl.create (Cudf.universe_size universe) in
(*  let add h k v =
     try let l = Hashtbl.find h k in l := v::!l
     with Not_found -> Hashtbl.add h k (ref [v])
  in *)
  Cudf.iter_packages (fun pkg ->
    let source = Cudf.lookup_package_property pkg "source" in
    let sourceversion = Cudf.lookup_package_property pkg "sourceversion" in
    try
      let hversions = Hashtbl.find th source in
      begin try
        let l = Hashtbl.find hversions sourceversion in
        l := pkg :: !l
      with Not_found ->
        Hashtbl.add hversions sourceversion (ref [pkg])
      end
    with Not_found -> begin
      let hversions = Hashtbl.create 17 in
      Hashtbl.add hversions sourceversion (ref [pkg]);
      Hashtbl.add th source hversions
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
