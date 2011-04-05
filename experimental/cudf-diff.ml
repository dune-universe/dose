(**************************************************************************************)
(*  Copyright (C) 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2010 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

module StringSet = Set.Make (String)

open ExtLib
open Common

module Cudf_set = CudfAdd.Cudf_set

type solution = {
  installed : Cudf_set.t ;
  removed : Cudf_set.t ;
  unchanged : Cudf_set.t
}

module Options = struct
  open OptParse
  let description = 
    "Compare two or more solutions.\n"^
    "cudf-diff problemfile solver1:solutionfile1 solver2:solutionfile2 ..."
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)
end

let pkg_names univ =
  Cudf.fold_packages (fun names pkg ->
    StringSet.add pkg.Cudf.package names
  ) StringSet.empty univ

let makeset l = List.fold_right Cudf_set.add l Cudf_set.empty

(* the list of all packages (versions) that were installed before
 * but not now *)
let removed univ sol pkgname =
  let were_installed = makeset (Cudf.get_installed univ pkgname) in
  let are_installed = makeset (Cudf.get_installed sol pkgname) in
  Cudf_set.diff were_installed are_installed

(* the list of all packages (versions) that were not installed before
 * but are installed now *)
let installed univ sol pkgname =
  let were_installed = makeset (Cudf.get_installed univ pkgname) in
  let are_installed = makeset (Cudf.get_installed sol pkgname) in
  Cudf_set.diff are_installed were_installed

(* for each pkgname I've the list of all versions that were installed, removed
 * or left unchanged *)
let diff univ sol =
  let pkgnames = pkg_names univ in
  let h = Hashtbl.create (StringSet.cardinal pkgnames) in
  StringSet.iter (fun pkgname ->
    let a = makeset (Cudf.lookup_packages univ pkgname) in
    let r = removed univ sol pkgname in
    let i = installed univ sol pkgname in
    let u = Cudf_set.diff a (Cudf_set.union r i) in
    let s = { removed = r ; installed = i ; unchanged = u } in
    Hashtbl.add h pkgname s
  ) pkgnames ;
  h

let pp_set ~inst fmt s =
  let install pkg = if pkg.Cudf.installed && inst then "*" else "" in
  let rec aux fmt s =
    if (Cudf_set.cardinal s) = 1 then
      let v = Cudf_set.min_elt s in
      Format.fprintf fmt "@,%d%s" 
      v.Cudf.version (install v)
    else begin
      let v = Cudf_set.min_elt s in
      Format.fprintf fmt "@,%d%s," v.Cudf.version (install v);
      aux fmt (Cudf_set.remove v s) 
    end
  in
  if Cudf_set.is_empty s then ()
  else Format.fprintf fmt "@[%a@]" aux s

let rec pp_list ?(sep="") pp_element fmt = function
  |[h] -> Format.fprintf fmt "%a" pp_element h
  |h::t ->
      Format.fprintf fmt "%a%s@,%a"
      pp_element h sep (pp_list ~sep pp_element) t
  |[] -> ()
;;

let pp_cell fmt cell = Format.fprintf fmt "%s" cell

let pp_header widths fmt header =
  let first_row = Array.map (fun x -> String.make (x + 1) ' ') widths in
  Array.iteri (fun j cell ->
    Format.pp_set_tab fmt ();
    for z=0 to (Buffer.length header.(j)) - 1 do cell.[z] <- Buffer.nth header.(j) z  done;
    Format.fprintf fmt "%s" cell
  ) first_row
;;

let pp_row pp_cell fmt row =
  Array.iteri (fun j cell ->
    Format.pp_print_tab fmt ();
    Format.fprintf fmt "%a" pp_cell cell
  ) row
;;

let pp_tables pp_row fmt (header,table) =
  (* we build with the largest length of each column of the 
   * table and header *)
  let widths = Array.create (Array.length table.(0)) 0 in
  Array.iter (fun row ->
    Array.iteri (fun j cell ->
      widths.(j) <- max (Buffer.length cell) widths.(j)
    ) row
  ) table;
  Array.iteri (fun j cell ->
    widths.(j) <- max (Buffer.length cell) widths.(j)
  ) header;

  (* open the table box *)
  Format.pp_open_tbox fmt ();

  (* print the header *)
  Format.fprintf fmt "%a" (pp_header widths) header;
  (* print the table *)
  Array.iter (pp_row fmt) table;

  (* close the box *)
  Format.pp_close_tbox fmt ();
;;

type t =
  |Un of Cudf_set.t (* unchanged *)
  |Rm of Cudf_set.t (* removed *)
  |In of Cudf_set.t (* installed *)
  |Up of Cudf_set.t (* upgraded *)
  |Dw of Cudf_set.t (* downgraded *)

let uniqueversion all s =
  let l = ref [] in
  let i = Cudf_set.filter (fun pkg -> pkg.Cudf.installed) all in
  if (Cudf_set.cardinal i <= 1) && ((Cudf_set.cardinal s.installed) <= 1) then
    begin
      if (Cudf_set.cardinal s.installed) = 1 then begin
        if (Cudf_set.cardinal i) = 1 then begin
          let np = Cudf_set.choose i in
          let op = Cudf_set.choose s.installed in
          if np.Cudf.version < op.Cudf.version
          then l := Up(s.installed)::!l 
          else l := Dw(s.installed)::!l
        end
        else
          l := In(s.installed)::!l
      end;
      if not (Cudf_set.is_empty s.unchanged) then l := Un(s.unchanged)::!l;
      if not (Cudf_set.is_empty s.removed) then l := Rm(s.removed)::!l ;
    end
  else begin
    if not (Cudf_set.is_empty s.unchanged) then l := Un(s.unchanged)::!l;
    if not (Cudf_set.is_empty s.removed) then l := Rm(s.removed)::!l ;
    if not (Cudf_set.is_empty s.installed) then l := In(s.installed)::!l ;
  end;
  !l
;;

let unchanged sols all pkgname =
  Array.exists (fun (solname,(filename,h)) ->
    let s = Hashtbl.find h pkgname in
    not(Cudf_set.equal all s.unchanged)
  ) sols

let allequal sols pkgname =
  let (solname,(filename,h)) = sols.(0) in
  let ss = Hashtbl.find h pkgname in
  Array.exists (fun (solname,(filename,h)) ->
    let s = Hashtbl.find h pkgname in
    not(
      (Cudf_set.equal s.installed ss.installed) &&
      (Cudf_set.equal s.removed ss.removed) &&
      (Cudf_set.equal s.unchanged ss.unchanged)
    )
  ) sols

let pp_diff fmt (univ,hl) =
  let header = Array.init ((List.length hl) + 1) (fun _ -> Buffer.create 50) in
  let a_hl = Array.of_list hl in
  Format.bprintf header.(0) "package names";
  Array.iteri (fun i (solname,_) -> Format.bprintf header.(i+1) "%s" solname) a_hl;
  let names = pkg_names univ in
  let table = 
    Array.init (StringSet.cardinal names) (fun _ ->
      Array.init ((List.length hl) + 1) (fun _ -> Buffer.create 50)
    )
  in
  let i = ref 0 in
  StringSet.iter (fun pkgname ->
    let all = makeset (Cudf.lookup_packages univ pkgname) in
    if allequal a_hl pkgname then begin
      Format.bprintf table.(!i).(0) "%s{%a}" pkgname (pp_set ~inst:true) all;
      for j = 0 to (List.length hl) - 1 do
        let (solname,(filename,h)) = a_hl.(j) in
        let s = Hashtbl.find h pkgname in
        let pp_elem fmt = function
          |In s -> Format.fprintf fmt "In{%a}" (pp_set ~inst:true) s
          |Un s -> Format.fprintf fmt "Un{%a}" (pp_set ~inst:true) s
          |Rm s -> Format.fprintf fmt "Rm{%a}" (pp_set ~inst:false) s
          |Dw s -> Format.fprintf fmt "Dw{%a}" (pp_set ~inst:true) s
          |Up s -> Format.fprintf fmt "Up{%a}" (pp_set ~inst:true) s
        in
        let l = uniqueversion all s in
        Format.bprintf (table.(!i).(j+1)) "@[<h>%a@]" (pp_list ~sep:"," pp_elem) l  
      done;
    end;
    incr i;
  ) names;
  let pp_buffer fmt t = Format.fprintf fmt "%s" (Buffer.contents t) in 
  Format.fprintf fmt "@[%a@]" (pp_tables (pp_row pp_buffer)) (header,table)
;;

let parse_univ f1 =
  match Boilerplate.load_cudf f1 with
  |_,_,None -> fatal "file %s is not a valid cudf document" f1
  |_,u,Some r -> u,r
;;

let check_sol u r s =
  match Cudf_checker.is_solution (u,r) s with
  |false,reasonlist ->
      (List.iter (fun r ->
        Printf.eprintf "%s\n" (Cudf_checker.explain_reason r)
      ) reasonlist;
      false)
  |true,_ -> true
;;

let main () =
  let posargs = OptParse.OptParser.parse_argv Options.options in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [];

  match posargs with
  |[] -> (Printf.eprintf "You must specify at least a universe and a solution\n" ; exit 1)
  |[u] -> (Printf.eprintf "You must specify at least a solution\n" ; exit 1)
  |u::l ->
      let (univ,req) = parse_univ u in
      let hl =
        List.filter_map (fun f ->
          let (h,f) =
            match Str.split (Str.regexp ":") f with
            |[f] -> (f,f)
            |[h;f] -> (h,f)
            |_ -> assert false
          in
          let (_,s,_) = Boilerplate.load_cudf f in
          if check_sol univ req s then Some (h,(f,s))
          else (Printf.eprintf "%s is not a valid solution. Discarded\n" f ; None)
        ) l
      in
      let sollist = List.map (fun (h,(f,s)) -> (h,(f,diff univ s))) hl in
      let fmt = Format.std_formatter in
      Format.fprintf fmt "@[%a@]@." pp_diff (univ,sollist);
;;

main ();;
