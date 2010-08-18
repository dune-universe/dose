(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Common

type reason =
  |Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
  |EmptyDependency of (Cudf.package * Cudf_types.vpkg list)
  |Conflict of (Cudf.package * Cudf.package)

type request =
  |Package of Cudf.package
  |PackageList of Cudf.package list

type result =
  |Success of (unit -> Cudf.package list)
  |Failure of (unit -> reason list)

type diagnosis = { result : result ; request : request }

(** given a list of dependencies, return a list of list containg all
 *  paths in the dependency tree starting from [root] *)
let build_paths deps root =
  let bind m f = List.flatten (List.map f m) in
  let rec aux acc deps root =
    match List.partition (fun (i,_,_) -> CudfAdd.equal i root) deps with
    |([],_) when (List.length acc) = 1 -> [] 
    |([],_) -> [List.rev acc]
    |(rootlist,rest) ->
        bind rootlist (function
          |(i,v,[]) -> [List.rev ((i,v)::acc)]
          |(i,v,l) -> bind l (fun r -> aux ((i,v)::acc) deps r)
        )
  in
  aux [] deps root
;;

let pp_package pp fmt pkg =
  let (p,v) = pp pkg in
  Format.fprintf fmt "package: %s@," (CudfAdd.decode p);
  Format.fprintf fmt "version: %s" v
;;

let pp_dependency pp fmt (i,vpkgs) =
  let pp_vpkglist fmt = 
    (* from libcudf ... again *)
    let pp_list fmt ~pp_item ~sep l =
      let rec aux fmt = function
        | [] -> assert false
        | [last] -> (* last item, no trailing sep *)
            Format.fprintf fmt "@,%a" pp_item last
        | vpkg :: tl -> (* at least one package in tl *)
            Format.fprintf fmt "@,%a%s" pp_item vpkg sep ;
            aux fmt tl
      in
      match l with
      | [] -> ()
      | [sole] -> pp_item fmt sole
      | _ -> Format.fprintf fmt "@[<h>%a@]" aux l
    in
    let string_of_relop = function
        `Eq -> "="
      | `Neq -> "!="
      | `Geq -> ">="
      | `Gt -> ">"
      | `Leq -> "<="
      | `Lt -> "<"
    in
    let pp_item fmt = function
      |(p,None) -> Format.fprintf fmt "%s" (CudfAdd.decode p)
      |(p,Some(c,v)) ->
          let (p,v) = pp {Cudf.default_package with Cudf.package = p ; version = v} in
          Format.fprintf fmt "%s (%s %s)" (CudfAdd.decode p) (string_of_relop c) v
    in
    pp_list fmt ~pp_item ~sep:" | "
  in
  Format.fprintf fmt "%a" (pp_package pp) i;
  if vpkgs <> [] then
    Format.fprintf fmt "@,vpkg: %a" pp_vpkglist vpkgs;
;;

let rec pp_list pp fmt = function
  |[h] -> Format.fprintf fmt "@[<v 1>-@,%a@]" pp h
  |h::t ->
      (Format.fprintf fmt "@[<v 1>-@,%a@]@," pp h ;
      pp_list pp fmt t)
  |[] -> ()
;;

let pp_dependencies pp root fmt deps =
  let dl = List.map (function Dependency x -> x |_ -> assert false) deps in
  let pathlist = build_paths dl root in
  let rec aux fmt = function
    |[path] -> Format.fprintf fmt "@[<v 1>-@,@[<v 1>path:@,%a@]@]" (pp_list (pp_dependency pp)) path
    |path::pathlist ->
        (Format.fprintf fmt "@[<v 1>-@,@[<v 1>path:@,%a@]@]@," (pp_list (pp_dependency pp)) path;
        aux fmt pathlist)
    |[] -> ()
  in
  aux fmt pathlist
;;

let print_error pp root fmt l =
  let (deps,res) = List.partition (function Dependency _ -> true |_ -> false) l in
  let pp_reason fmt = function
    |Conflict (i,j) ->
        Format.fprintf fmt "@[<v 1>conflict:@,";
        Format.fprintf fmt "@[<v 1>pkg1:@,%a@]@," (pp_package pp) i;
        Format.fprintf fmt "@[<v 1>pkg2:@,%a@]" (pp_package pp) j;
        if deps <> [] then begin
          let dl = Dependency(i,[],[])::Dependency(j,[],[])::deps in
          Format.fprintf fmt "@,@[<v 1>paths:@,%a@]" (pp_dependencies pp root) dl;
          Format.fprintf fmt "@]"
        end else
          Format.fprintf fmt "@,@]"
    |EmptyDependency (i,vpkgs) ->
        Format.fprintf fmt "@[<v 1>missing:@,";
        Format.fprintf fmt "@[<v 1>pkg:@,%a@]" (pp_dependency pp) (i,vpkgs);
        if deps <> [] then begin
          let dl = Dependency(i,vpkgs,[])::deps in
          Format.fprintf fmt "@,@[<v 1>paths:@,%a@]" (pp_dependencies pp root) dl;
          Format.fprintf fmt "@]"
        end else
          Format.fprintf fmt "@,@]"

    |_ -> assert false
  in
  pp_list pp_reason fmt res;
;;

let default_pp pkg = (pkg.Cudf.package, CudfAdd.string_of_version pkg)

let print ?(pp=default_pp) ?(failure=false) ?(success=false) ?(explain=false) fmt = function
  |{result = Success (f); request = Package r } when success ->
       Format.fprintf fmt "@[<v 1>-@,";
       Format.fprintf fmt "@[<v>%a@]@," (pp_package pp) r;
       Format.fprintf fmt "status: ok";
       if explain then begin
         let is = f () in
         if is <> [] then
           Format.fprintf fmt "@,installationset:@,%a" (pp_list (pp_package pp)) is;
       end;
       Format.fprintf fmt "@]@,"
  |{result = Failure (f) ; request = Package r } when failure -> 
       Format.fprintf fmt "@[<v 1>-@,";
       Format.fprintf fmt "@[<v>%a@]@," (pp_package pp) r;
       Format.fprintf fmt "status: broken@,";
       if explain then begin
         Format.fprintf fmt "@[<v 1>reasons:@,";
         Format.fprintf fmt "@[<v>%a@]" (print_error pp r) (f ());
         Format.fprintf fmt "@]"
       end;
       Format.fprintf fmt "@]@,"
  |_ -> ()
;;

let is_solution = function
  |{result = Success _ } -> true
  |{result = Failure _ } -> false
