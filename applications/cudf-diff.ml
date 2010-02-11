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

open Cudf
open ExtLib
open Common
open CudfAdd

type status = 
  |Upgraded of Cudf.package (* there exists a newer version in the solution *)
  |Downgraded of Cudf.package (* there exists an older version in the solution *)
  |Installed (* this package was not installed before *)
  |Removed (* this package is not installed anymore *)
  |Unchanged (* this package was installed before and it is now *)

type cmp_t = Up | Down | Inst | Rem | Un

type solution = {
  mutable removed : int ;
  mutable downgraded : int;
  mutable upgraded : int;
  mutable newinst : int;
  mutable unchanged : int;
}

module Options = struct
  open OptParse

  exception Format
  let out_option ?default ?(metavar = "<txt|html>") () =
    let corce = function
      |"txt" -> "txt"
      |"html" -> "html"
      | _ -> raise Format
    in
    let error _ s = Printf.sprintf "%s format not supported" s in
    Opt.value_option metavar default corce error

  let order_option ?default ?(metavar = "<Un:Up:Down:Inst:Rem>") () =
    let corce s =
      let sl = Str.split (Str.regexp ":") s in
      if sl = [] then raise Format
      else
        List.map (function
          |"Up" -> Up
          |"Un" -> Un
          |"Down" -> Down
          |"Inst" -> Inst
          |"Rem" -> Rem
          | _ -> raise Format
        ) sl
    in
    let error _ s = Printf.sprintf "%s ordering not supported" s in
    Opt.value_option metavar default corce error

  let debug = StdOpt.store_true ()
  let output = out_option ~default:"txt" ()
  let order = order_option ~default:[Un;Up;Down;Inst;Rem] ()

  let description = "Compare two or more solutions. Format : solvername:solutionfile"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'t' ~long_name:"output" ~help:"Output type" output;
  add options ~short_name:'o' ~long_name:"order" ~help:"Order" order;
end

let compare_lex l s1 s2 =
  List.fold_left (fun acc -> function
    |Un -> s1.unchanged - s2.unchanged
    |Up -> s1.upgraded - s2.upgraded
    |Inst -> s1.newinst - s2.newinst
    |Down -> s1.downgraded - s2.downgraded
    |Rem -> s1.removed - s2.removed
  ) 0 l

let dummy_solution () = {
  removed = 0 ;
  downgraded = 0;
  upgraded = 0;
  newinst = 0;
  unchanged = 0;
}

let status_to_string pkg = function
  |Upgraded p -> Printf.sprintf "Upgraded from %d to %d" pkg.version p.version
  |Downgraded p -> Printf.sprintf "Downgraded from %d to %d" pkg.version p.version
  |Installed -> "New"
  |Removed -> "Removed"
  |Unchanged -> "Unchanged"

let solution_to_string s =
  Printf.sprintf
  "removed = %d\nupgraded = %d\ndowngraded = %d\nnew = %d\nunchanged = %d\n"
  s.removed s.upgraded s.downgraded s.newinst s.unchanged
;;

let solution_to_html s =
  Printf.sprintf
  "<td>removed = %d<br>upgraded = %d<br>downgraded = %d<br>new = %d<br>unchanged = %d</td>"
  s.removed s.upgraded s.downgraded s.newinst s.unchanged
;;

let status_equal = function
  |(Removed,Removed)
  |(Installed,Installed)
  |(Unchanged,Unchanged) -> true
  |((Upgraded p1,Upgraded p2)|(Downgraded p1,Downgraded p2)) when CudfAdd.equal p1 p2 -> true
  |_,_ -> false

let all_equal = function
  |[_] -> true
  |one::t -> List.for_all (fun sc -> status_equal (sc,one)) t
  |[] -> failwith "Empty list"

(* a packages matching vpkg in the universe u *)
let provides u vpkg =
  List.map (fun (pkg,_) -> pkg) (Cudf.who_provides u vpkg)

(* for each provide of pkg return all concrete package providing the same functionality *)
let provides_list u pkg =
  List.map (fun vpkg ->
    provides u (vpkg :> Cudf_types.vpkg) 
  ) pkg.provides

let diff univ sol =
  let h = Cudf_hashtbl.create (Cudf.universe_size univ) in
  let s = dummy_solution () in

  Cudf.iter_packages (fun pkg ->
    if pkg.installed then
      let l = Cudf.lookup_packages sol pkg.package in
      match List.filter (fun pkg -> pkg.installed) l with
      |[] -> (s.removed<-s.removed+1 ; Cudf_hashtbl.add h pkg Removed)
      |l ->
          List.iter (fun p ->
            if Cudf.version_matches p.version (Some(`Eq,pkg.version))
            then begin s.unchanged<-s.unchanged+1 ; Cudf_hashtbl.add h pkg Unchanged end
            else if Cudf.version_matches p.version (Some(`Gt,pkg.version))
            then begin s.upgraded<-s.upgraded+1 ; Cudf_hashtbl.add h pkg (Upgraded p) end
            else if Cudf.version_matches p.version (Some(`Lt,pkg.version))
            then begin s.downgraded<-s.downgraded+1 ; Cudf_hashtbl.add h pkg (Downgraded p) end
          ) l
    else
      try
        let p = Cudf.lookup_package sol (pkg.package,pkg.version) in
        if p.installed then begin s.newinst<-s.newinst+1 ; Cudf_hashtbl.add h pkg Installed end
      with Not_found -> ()
  ) univ
  ;
  (h,s)
;;

let print_diff_txt universe solutions =
  let cmp (_,(_,(_,s1))) (_,(_,(_,s2))) = compare_lex (OptParse.Opt.get Options.order) s1 s2 in
  let solutions = List.sort ~cmp:cmp solutions in 
  let (hl,solutions) = List.split solutions in
  Printf.printf "Package | %s\n" (String.concat " | " hl) ;
  Cudf.iter_packages (fun pkg ->
    let pl = 
      List.filter_map (fun (f,(h,_)) ->
        try Some(f,Cudf_hashtbl.find h pkg) with Not_found -> None
      ) solutions
    in
    if pl = [] then () else
    if all_equal (List.map snd pl) then ()
    else begin
      let sl = List.map (fun (f,status) -> status_to_string pkg status) pl in
      Printf.printf "%s %d | %s\n" pkg.package pkg.version (String.concat " | " sl)
    end
  ) universe
;;

let print_diff_html universe solutions =
  let cmp (_,(_,(_,s1))) (_,(_,(_,s2))) = compare_lex (OptParse.Opt.get Options.order) s1 s2 in
  let solutions = List.sort ~cmp:cmp solutions in 
  let (hl,solutions) = List.split solutions in
  Printf.printf "<table border=1>\n" ;
  Printf.printf "<tr><tbody><th>Package</th>%s</tr>"
  (String.concat "" (List.map (fun h -> Printf.sprintf "<th>%s</th>" h) hl));
  Cudf.iter_packages (fun pkg ->
    let pl = 
      List.filter_map (fun (f,(h,_)) ->
        try Some(f,Cudf_hashtbl.find h pkg) with Not_found -> None
      ) solutions
    in
    if pl = [] then () else
    if all_equal (List.map snd pl) then ()
    else begin
      let sl = List.map (fun (f,status) -> status_to_string pkg status) pl in
      Printf.printf "<tr><td>%s %d</td>%s</tr>"
      pkg.package pkg.version
      (String.concat "" (List.map (fun s -> Printf.sprintf "<td>%s</td>" s) sl))
    end
  ) universe
  ;
  Printf.printf "</table>\n"
;;


let parse_univ f1 =
  match CudfAdd.load_cudf f1 with
  |_,_,None -> 
      (Printf.eprintf "file %s is not a valid cudf document\n" f1 ; exit 1)
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
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;

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
          let (_,s,_) = CudfAdd.load_cudf f in
          if check_sol univ req s then Some (h,(f,s))
          else (Printf.eprintf "%s is not a valid solution. Discarded\n" f ; None)
        ) l
      in
      let sol_tables = List.map (fun (h,(f,s)) -> (h,(f,diff univ s))) hl in
      match OptParse.Opt.get Options.output with
      |"txt" -> print_diff_txt univ sol_tables 
      |"html" -> print_diff_html univ sol_tables
      |_ -> assert false 
;;

main ();;
