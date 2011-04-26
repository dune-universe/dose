let load url =
begin
	let (univ, _, _) = Boilerplate.load_universe [url] in
	univ
end;;

let search_package u re =
  let rex = Pcre.regexp re in
   Cudf.get_packages 
    ~filter:(fun p -> Pcre.pmatch ~rex (Cudf_types_pp.string_of_pkgname p.Cudf.package))
		u
;;

let trim u = Algo.Depsolver.trim u;;

let cone ?maxdepth ?conjunctive u pl =
  Algo.Depsolver.dependency_closure ?maxdepth ?conjunctive u pl 
;;

let rev_cone ?maxdepth u pl =
  Algo.Depsolver.reverse_dependency_closure ?maxdepth u pl 
;;

let filter_packages f u =
	List.rev (Cudf.fold_packages (fun acc p -> if f p then p::acc else acc) [] u)
;;

(* examples:
let u = load "deb:///var/lib/apt/lists/ftp.be.debian.org_debian_dists_squeeze_main_binary-amd64_Packages";;
let ocamlunits= search_package u "^ocaml";;
let c12 = let (a::b::_) = ocamlunits in cone u [a;b];; (* union des cones de 2 paquets Ocaml *)
 *)
