(**************************************************************************************)
(*  Copyright (C) 2011 Pietro Abate, Roberto Di Cosmo                                 *)
(*  Copyright (C) 2011 Mancoosi Project                                               *)
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

type range = [
    `Hi of string
  | `In of (string * string)
  | `Lo of string 
  | `Eq of string
]

let string_of_range = function
  |`Hi v -> Printf.sprintf "%s < ." v
  |`Lo v -> Printf.sprintf ". < %s" v
  |`Eq v -> Printf.sprintf "= %s" v
  |`In (v1,v2) -> Printf.sprintf "%s < . < %s" v1 v2
;;

let evalsel compare (target,constr) =
  match target with
  |`Hi v ->
      begin match constr with
      |(`Gt,w) -> compare v w > 1
      |_ -> false end
  |`Lo v ->
      begin match constr with
      |(`Lt,w) -> compare v w < 1
      |_ -> false end
  |`Eq v ->
      begin match constr with
      |(`Eq,w) -> compare v w = 0
      |_ -> false end
  |`In (v1,v2) ->
      begin match constr with
      |(`Eq,w) -> (compare v1 w > 1) && (compare v2 w < 1)
      |((`Gt|`Geq),w) ->
          (compare v1 w = 0) || (compare v1 w > 1) && (compare v2 w < 1) 
      |((`Lt|`Leq),w) ->
          (compare v2 w = 0) || (compare v1 w > 1) && (compare v2 w < 1) 
      |_ -> false end
;;

let range vl =
  let l = List.sort ~cmp:(fun v1 v2 -> Version.compare v2 v1) vl in
  let rec aux acc = function
    |(None,[]) -> acc
    |(None,a::t) -> aux ((`Hi a)::acc) (Some a,t)
    |(Some b,a::t) -> aux ((`In (a,b))::(`Eq b)::acc) (Some a,t)
    |(Some b,[]) -> (`Lo b)::(`Eq b)::acc
  in
  aux [] (None,l)
;;

(* vl \subseteq realversions ( constraints ) *)
let discriminant vl constraints =
  let eval_constr = Hashtbl.create 17 in
  let constr_eval = Hashtbl.create 17 in
  List.iter (fun target ->
    let eval =
      List.map (fun constr -> 
        evalsel Version.compare (target,constr)
      ) constraints 
    in
    try
      let v_rep = Hashtbl.find eval_constr eval in
      let l = Hashtbl.find constr_eval v_rep in
      Hashtbl.replace constr_eval v_rep (target::l)
    with Not_found -> begin
      Hashtbl.add eval_constr eval target;
      Hashtbl.add constr_eval target []
    end
  ) (range vl) ;
  constr_eval
;;

let add_unique h k v =
  try
    let vh = Hashtbl.find h k in
    if not (Hashtbl.mem vh v) then
      Hashtbl.add vh v ()
  with Not_found -> begin
    let vh = Hashtbl.create 17 in
    Hashtbl.add vh v ();
    Hashtbl.add h k vh
  end

(* collect dependency information *)
let conj_iter f t l =
  List.iter (fun (name,sel) ->
    match CudfAdd.cudfop sel with
    |None -> ()
    |Some(c,v) -> add_unique t name (f (c,v))
  ) l
let cnf_iter f t ll = List.iter (conj_iter f t) ll

(** [constraints universe] returns a map between package names
    and an ordered list of constraints where the package name is
    mentioned *)
let constraints packagelist =
  let id x = x in
  let constraints_table = Hashtbl.create (List.length packagelist) in
  List.iter (fun pkg ->
    conj_iter id constraints_table pkg.Packages.conflicts ;
    conj_iter id constraints_table pkg.Packages.provides ;
    cnf_iter id constraints_table pkg.Packages.depends
  ) packagelist
  ;
  let h = Hashtbl.create (List.length packagelist) in
  let elements hv =
    List.sort ~cmp:(fun (_,v1) (_,v2) -> Version.compare v2 v1) (
      Hashtbl.fold (fun k v acc -> k::acc) hv []
    )
  in
  Hashtbl.iter (fun n hv -> Hashtbl.add h n (elements hv)) constraints_table;
  h
;;

let all_constraints table pkgname =
  try Hashtbl.find table pkgname
  with Not_found -> []
;;

(* return a new target rebased accordingly to the epoch of the base version *)
let align version target =
  match Version.split version  with
  |("",_,_,_) -> target
  |(pe,_,_,_) ->
    let rebase v =
      match Version.split v with
      |(_,u,"","") -> Printf.sprintf "%s:%s" pe u
      |(_,u,r,"")  -> Printf.sprintf "%s:%s-%s" pe u r
      |(_,u,r,b)   -> Printf.sprintf "%s:%s-%s%s" pe u r b
    in
    match target with
    |`Eq v -> `Eq (rebase v)
    |`Hi v -> `Hi (rebase v)
    |`Lo v -> `Lo (rebase v)
    |`In (v,w) -> `In (rebase v,rebase w)
;;

let all_versions constr = Util.list_unique (List.map (snd) constr) ;;

(* downgrade establish the upgrade treshold from which the discriminant should
 * be considered as an upgrade *) 
let discriminants ?downgrade constraints_table cluster =
  Util.list_unique (
    List.fold_left (fun l pkg ->
      let constr = all_constraints constraints_table pkg.Packages.name in
      let uplist = 
        let vl = all_versions constr in
        match downgrade with
        |None -> vl
        |Some v -> List.filter (fun w -> (Version.compare v w) < 0) vl 
      in
      let d = discriminant uplist constr in
      (Hashtbl.fold (fun k v acc -> k::acc) d []) @ l
    ) [] cluster
  )
;;

let migrate packagelist target =
  List.map (fun pkg -> ((pkg,target),(align pkg.Packages.version target))) packagelist
;;

(*
let main () =
  let packagelist = Packages.input_raw ["tests/discriminants"] in
  let constraints_table = constraints packagelist in
  List.iter (fun pkg ->
    Printf.eprintf "Name %s\nVersion %s\n" pkg.Packages.name pkg.Packages.version;
    let constr = all_constraints constraints_table pkg.Packages.name in
    List.iter (fun (c,s) ->
      let p = function
        |`Eq -> "=" | `Geq -> ">=" | `Gt -> ">" | `Leq -> "=<" | `Lt -> "<"
      in 
      Printf.eprintf "Constr (%s,%s) \n" (p c) s
    ) constr ;
    let vl = all_versions constr in
    let rl = range ~downgrade:false vl in
    List.iter (fun r ->
      let p = function
        |`Eq s -> Printf.eprintf " %s " s
        |`Hi s -> Printf.eprintf " . < %s " s
        |`Lo s -> Printf.eprintf " %s < . " s
        |`In (v1,v2) -> Printf.eprintf " %s < . < %s " v1 v2
      in
      Printf.eprintf "range "; p r ;
      Printf.eprintf "\n"
    ) rl;
    List.iter (fun v ->
      Printf.eprintf "Versions %s\n" v
    ) vl;
    let d = discriminant vl constr in
    Hashtbl.iter (fun k v ->
      let p = function
        |`Eq s -> Printf.eprintf " %s " s
        |`Hi s -> Printf.eprintf " . < %s " s
        |`Lo s -> Printf.eprintf " %s < . " s
        |`In (v1,v2) -> Printf.eprintf " %s < . < %s " v1 v2
      in
      p k;
      Printf.eprintf "[";
      List.iter p v;
      Printf.eprintf "]\n"
    ) d 
  ) packagelist 
;;

main ();;  
*)


