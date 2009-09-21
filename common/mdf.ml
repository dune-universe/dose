
open CudfAdd

type package = {
  id : int ;
  pkg : Cudf.package;
  depends : (Cudf_types.vpkg list * int array * Cudf.package list) array ;
  conflicts : (Cudf.package * int) array
}

type universe = {
  cudf : Cudf.universe ;
  index : package array ;
  maps : CudfAdd.maps
}

let default_package = {
  id = 0;
  pkg = Cudf.default_package;
  depends = [||] ;
  conflicts = [||] 
}

let __load maps universe =
  let to_sat = maps.map#vartoint in
  let a = Array.create (Cudf.universe_size universe) default_package in
  Cudf.iter_packages (fun pkg ->
    let id = to_sat pkg in
    let cl =
      List.map (fun p ->
        (p,to_sat p)
      ) (maps.who_conflicts pkg)
    in
    let dll =
      List.map (fun disjunction ->
        List.fold_left (fun (l1,l2,l3) vpkg ->
          let dl = (maps.lookup_packages vpkg) in
          let el = Array.of_list (List.map to_sat dl) in
          (vpkg::l1,Array.append el l2, dl @ l3)
        ) ([],[||],[]) disjunction
      ) pkg.Cudf.depends
    in
    let p = {
      id = id;
      pkg = pkg;
      depends = Array.of_list dll;
      conflicts = Array.of_list cl
    }
    in
    a.(id) <- p
  ) universe;
  a

let load_from_list pkglist =
  let universe = Cudf.load_universe pkglist in
  let maps = build_maps universe in

  let index = __load maps universe in
  { cudf = universe ; index = index ; maps = maps }

let load_from_universe universe =
  let maps = build_maps universe in
  let index = __load maps universe in
  { cudf = universe ; index = index ; maps = maps }

(*
let dump mdf =
  let index = mdf.index in
  for i=0 to mdf.maps.size - 1; do
    Printf.eprintf "%d -> %s\n%!" i (CudfAdd.print_package index.(i).pkg);
  done
*)
