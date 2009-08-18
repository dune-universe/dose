
open ExtLib

let print_package ?(short=true) pkg =
  if short then
    let (sp,sv) =
      try (pkg.Cudf.package,Cudf.lookup_package_property pkg "number")
      with Not_found -> (pkg.Cudf.package,string_of_int pkg.Cudf.version)
    in Printf.sprintf "(%s,%s)" sp sv
  else
    Cudf_printer.string_of_package pkg

type reason =
  |Dependency of (Cudf.package * Cudf.package list)
  |EmptyDependency of (Cudf.package * Cudf_types.vpkg list)
  |Conflict of (Cudf.package * Cudf.package)

  |Installed_alternatives of Cudf.package list
  |To_install of (Cudf_types.vpkg * Cudf.package list)
  |To_remove of (Cudf_types.vpkg * Cudf.package)
  |To_upgrade of (Cudf_types.vpkg * Cudf.package list)
  |To_upgrade_singleton of (Cudf_types.vpkg * Cudf.package list)

type result =
  |Success of ( unit -> Cudf.package list )
  |Failure of ( unit -> reason list )

type request = 
  |Sng of Cudf.package
  |Lst of Cudf.package list
  |Req

type diagnosis = {
  request : request ;
  result : result
}

let print ?(explain=false) oc result = 
  let print_request = function
    |Sng p -> (print_package ~short:true p)
    |Lst pl -> String.concat "," (List.map (print_package ~short:true) pl)
    |Req -> ""
  in

  match result,explain with
  |{result = Failure (_) ; request = r },false ->
      Printf.fprintf oc "%s : FAILED\n" (print_request r)
  |{ result = Success (_); request = r },false ->
      Printf.fprintf oc "%s : SUCCESS\n" (print_request r)
  |{ result = Success (f); request = r },true ->
      begin
        Printf.fprintf oc "%s : SUCCESS\n" (print_request r) ;
        List.iter (fun p ->
          Printf.fprintf oc "%s\n" (print_package ~short:false p)
        ) (f ())
      end
  |{result = Failure (f) ; request = r },true -> 
     begin
       Printf.fprintf oc "%s : FAILED\n" (print_request r) ;
       List.iter (function
         |Dependency(i,l) ->
            let l = List.map (print_package ~short:true) l in
            Printf.fprintf oc
            "Dependency Error %s -> %s\n"
            (print_package ~short:true i)
            (String.concat " , " l)
         |Conflict (i,j) ->
            Printf.fprintf oc
            "Conflict %s <-#-> %s \n"
            (print_package ~short:true i) (print_package ~short:true j)
         |EmptyDependency (i,vpkgs) ->
            Printf.fprintf oc
            "Broken Dependency %s. %s cannot be satisfied\n"
            (print_package ~short:true i) 
            (String.concat " | " (
              List.map (fun vpkg -> (Cudf_types.string_of_vpkg vpkg)) vpkgs)
            )

         |Installed_alternatives(l) ->
            let l = List.map (print_package ~short:true) l in
            Printf.fprintf oc
            "There are not alternatives to replace the installed package(s) %s\n"
            (String.concat " , " l)

         |To_install(vpkg,[]) ->
            Printf.fprintf oc
            "You have requested to install %s, but no package in the current universe match the given contraint\n"
            (Cudf_types.string_of_vpkg vpkg)
         |To_install(vpkg,l) ->
            let l = List.map (print_package ~short:true) l in
            Printf.fprintf oc 
            "No alternatives for the installation of %s.\n The package %s is in the current universe, but it has broken dependencies\n"
            (Cudf_types.string_of_vpkg vpkg)
            (String.concat " , " l)

         |To_remove(vpkg,i) ->
            Printf.fprintf oc
            "You have requested to remove %s.  But I cannot remove %s\n"
            (Cudf_types.string_of_vpkg vpkg) (print_package ~short:true i)

         |To_upgrade(vpkg,l) ->
            let l = List.map (print_package ~short:true) l in
            Printf.fprintf oc
            "You have requested to upgrade %s. Candidate alternatives : %s\n"
            (Cudf_types.string_of_vpkg vpkg) (String.concat " , " l)
         |To_upgrade_singleton(vpkg,l) ->
            let l = List.map (print_package ~short:true) l in
            Printf.fprintf oc
            "Singleton constraint %s to upgrade for %s\n"
            (Cudf_types.string_of_vpkg vpkg) (String.concat " , " l)
       ) (f ())
   end
