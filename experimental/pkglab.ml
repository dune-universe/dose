let load url =
begin
	let (univ, _, _) = Boilerplate.load_universe [url] in
	univ
end;;

let search_package u re =
  let regex = Str.regexp re in
   Cudf.get_packages 
    ~filter:
    (fun p -> 
      try 
	ignore(Str.search_forward regex p.Cudf.package 0);true 
      with Not_found -> false)
    u
;;

let trim u = Algo.Depsolver.trim u;;

let cone ?maxdepth ?conjuntive u pl =
  Algo.Depsolver.dependency_closure ?maxdepth ?conjuntive u pl 
;;

let rev_cone ?maxdepth u pl =
  Algo.Depsolver.reverse_dependency_closure ?maxdepth u pl 
;;

(* examples:
let u = load "deb:///var/lib/apt/lists/ftp.be.debian.org_debian_dists_squeeze_main_binary-amd64_Packages";;
let ocamlunits= search_package u "^ocaml";;
let c12 = let (a::b::_) = ocamlunits in cone u [a;b];; (* union des cones de 2 paquets Ocaml *)
 *)
