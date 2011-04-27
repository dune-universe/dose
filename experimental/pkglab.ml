module Package =
struct
	type t = Cudf.package

	let compare = Cudf.(<%)
end;;
module PS = Set.Make(Package);;

let load url =
begin
	let (univ, _, _) = Boilerplate.load_universe [url] in
	let maps = Common.CudfAdd.build_maps univ in
	(univ, maps)
end;;

let search_package (u, m) re =
  let rex = Pcre.regexp re in
   Cudf.get_packages 
    ~filter:(fun p -> Pcre.pmatch ~rex (Cudf_types_pp.string_of_pkgname p.Cudf.package))
		u
;;

let trim (u, m) = Algo.Depsolver.trim u;;

let cone ?maxdepth ?conjunctive (u, m) pl =
  Algo.Depsolver.dependency_closure ?maxdepth ?conjunctive u pl 
;;

let rev_cone ?maxdepth (u, m) pl =
  Algo.Depsolver.reverse_dependency_closure ?maxdepth u pl 
;;

let filter_packages f (u, m) =
	List.rev (Cudf.fold_packages (fun acc p -> if f p then p::acc else acc) [] u)
;;

let provides_set (u, m) p =
	List.fold_left (fun acc (pn, pv) ->
		List.fold_left (fun acc' p' ->
			PS.add p' acc'
		) acc (m.Common.CudfAdd.who_provides (pn, (pv :> Cudf_types.constr)))
	) PS.empty p.Cudf.provides
;;

let conflicts_set (u, m) p =
	List.fold_left (fun acc (cn, cv) ->
		List.fold_left (fun acc' c ->
			PS.add c acc'
		) acc (Cudf.lookup_packages ~filter:cv u cn)
	) PS.empty p.Cudf.conflicts
;;

(* examples:
let u = load "deb:///var/lib/apt/lists/ftp.be.debian.org_debian_dists_squeeze_main_binary-amd64_Packages";;
let ocamlunits= search_package u "^ocaml";;
let c12 = let (a::b::_) = ocamlunits in cone u [a;b];; (* union des cones de 2 paquets Ocaml

let l = filter_packages (fun p -> not (PS.is_empty (PS.inter (provides_set u p) (conflicts_set u p)))) u;;
 *)
