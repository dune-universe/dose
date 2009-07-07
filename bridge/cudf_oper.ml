open ExtLib

(** additional Cudf indexes *)

module Make (Cudf : Cudf.T) = struct

  open Cudf
  open Cudf_types

  type maps = {
    who_conflicts : Cudf.package -> Cudf.package list;
    who_provides : Cudf_types.vpkg -> Cudf.package list
  }

  let build_maps universe =
    let size = Cudf.universe_size universe in
    let conflicts = Hashtbl.create (2 * size) in
    let provides = Hashtbl.create (2 * size) in
    Cudf.iter_packages (fun pkg ->
      List.iter (function
        |name, None -> Hashtbl.add provides name (pkg, None)
        |name, Some (_, ver) -> Hashtbl.add provides name (pkg, (Some ver))
      ) pkg.provides
    ) universe

    ;

    let who_provides (pkgname,constr) =
      match Cudf.lookup_packages ~filter:constr universe pkgname with
      |[] ->
          List.filter_map (function
            |pkg, None -> Some(pkg)
            |pkg, Some v when Cudf.version_matches v constr -> Some(pkg)
            |_,_ -> None
          ) (Hashtbl.find_all provides pkgname)
      |l -> l
    in

    Cudf.iter_packages (fun pkg ->
      List.iter (fun (name,constr) ->
        List.iter (fun p ->
          Hashtbl.add conflicts pkg p ;
          Hashtbl.add conflicts p pkg
        ) (who_provides (name,constr))
      ) pkg.conflicts
    ) universe
    ;

    let who_conflicts pkg = List.unique (Hashtbl.find_all conflicts pkg) in

    {
      who_conflicts = who_conflicts ;
      who_provides = who_provides ;
    }

end

