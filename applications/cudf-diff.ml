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

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()
  let solution = StdOpt.store_true ()
  let compare = StdOpt.store_true ()

  let description = "By default we show only the number of broken packages"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
  add options ~short_name:'s' ~long_name:"solution" ~help:"consider file2 as a solution to the cudf problem in file1 and print the complete list of installed packages" solution;
  add options ~short_name:'c' ~long_name:"compare" ~help:"compare solutions file2 file3 for the cudf problem in file1" compare;
end

type status = 
  |Upgraded of Cudf.package (* there exists a newer version in the solution *)
  |Downgraded of Cudf.package (* there exists an older version in the solution *)
  |Replaced of Cudf.package list list (* a list of package providing the same functionalities *)
  |Installed (* this package was not installed before *)
  |Removed (* this package is not installed anymore *)
  |Unchanged (* this package was installed before and it is now *)

type solution = {
  mutable replaced : int ;
  mutable removed : int ;
  mutable downgraded : int;
  mutable updated : int;
  mutable newinst : int;
}

let dummy_solution = {
  replaced = 0;
  removed = 0 ;
  downgraded = 0;
  updated = 0;
  newinst = 0;
}

let status_to_string pkg = function
  |Removed ->
      Printf.sprintf "Package (%s,%d) Removed" pkg.package pkg.version
  |Replaced l ->
      let sl = [] in 
      Printf.sprintf "Package (%s,%d) Replaced by [%s]"
      pkg.package pkg.version (* p.package p.version *) ""
  |Upgraded p ->
      Printf.sprintf "Package %s Upgraded from %d to %d"
      pkg.package pkg.version p.version
  |Downgraded p ->
      Printf.sprintf "Package %s Downgraded from %d to %d"
      pkg.package pkg.version p.version
  |Installed -> 
      Printf.sprintf "Package (%s,%d) New" pkg.package pkg.version
  |Unchanged -> "Unchanged"

let solution_to_string s =
  Printf.sprintf
  "removed = %d\nreplaced = %d\nupdated = %d\ndowngraded = %d\nnew = %d\n"
  s.removed s.replaced s.updated s.downgraded s.newinst
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
  let s = dummy_solution in

  Cudf.iter_packages (fun pkg ->
    if pkg.installed then
      let l = Cudf.lookup_packages sol pkg.package in
      match List.filter (fun pkg -> pkg.installed) l with
      |[] ->
(*          let l = provides_list sol pkg in
          if List.for_all (function [] -> false | _ -> true) l then begin
            s.replaced<-s.replaced+1;
            Cudf_hashtbl.add h pkg (Replaced l)
          end
          else
            *)
            Cudf_hashtbl.add h pkg Removed

      |l ->
          List.iter (fun p ->
            if Cudf.version_matches p.version (Some(`Eq,pkg.version))
            then Cudf_hashtbl.add h pkg Unchanged
            else if Cudf.version_matches p.version (Some(`Gt,pkg.version))
            then begin s.updated<-s.updated+1 ; Cudf_hashtbl.add h pkg (Upgraded p) end
            else if Cudf.version_matches p.version (Some(`Lt,pkg.version))
            then begin s.downgraded<-s.downgraded+1 ; Cudf_hashtbl.add h pkg (Downgraded p) end
          ) l
    else
      try
        let p = Cudf.lookup_package sol (pkg.package,pkg.version) in
        if p.installed then begin
          s.newinst<-s.newinst+1 ;
          Cudf_hashtbl.add h pkg Installed
        end
      with Not_found -> ()
  ) univ
  ;
  (h,s)
;;

let print_diff universe solutions =
  Cudf.iter_packages (fun pkg ->
    let pl = 
      List.filter_map (fun (f,(h,_)) ->
        try Some(f,Cudf_hashtbl.find h pkg) with Not_found -> None
      ) solutions
    in
    if pl = [] then () else
    if all_equal (List.map snd pl) then ()
    else begin
      Printf.printf "%s\n" (CudfAdd.print_package pkg);
      List.iter (function
        |f,status -> Printf.printf "%s: %s\n" f (status_to_string pkg status)
      ) pl;
      print_newline ()
    end
  ) universe
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
      let sol_list =
        List.filter_map (fun f ->
          let (_,s,_) = CudfAdd.load_cudf f in
          if check_sol univ req s then Some (f,s)
          else (Printf.eprintf "%s is not a valid solution. Discarded\n" f ; None)
        ) l
      in
      let sol_tables = List.map (fun (f,s) -> (f,diff univ s)) sol_list in
      print_diff univ sol_tables

      (*
      let changed1 = replaced1 + updated1 + downgraded1 in
      let changed2 = replaced2 + updated2 + downgraded2 in
      let paranoid = [`Min(removed1,removed2);`Min(changed1,changed2)] in
      let trendy = [`Min(removed1,removed2);`Max(updated1,updated2);`Max(newinst1,newinst2)] in
      Printf.printf "paranoid profile (install, min removed, min changed)\n" ;
      if compare paranoid = 1 then Printf.printf "Paranoid : The winner is %s\n" f2
      else if compare paranoid = -1 then Printf.printf "Paranoid : The winner is %s\n" f3
      else Printf.printf "Paranoid : Equivalent solutions\n"
      ;
      Printf.printf "trendy profile (install, min removed, max fresh, min new)\n" ;
      if compare trendy = 1 then Printf.printf "Trendy : The winner is %s\n" f2
      else if compare trendy = -1 then Printf.printf "Trendy : The winner is %s\n" f3
      else Printf.printf "Trendy : Equivalent solutions\n"
      *)
;;

main ();;
