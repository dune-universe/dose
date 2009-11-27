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

let conservative = ref true

let parse_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    (* Printf.eprintf "parsing CUDF ...\n%!"; *)
    Cudf_parser.load p
  with
    Cudf_parser.Parse_error _
    | Cudf.Constraint_violation _ as exn ->
      Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
      doc (Printexc.to_string exn);
      exit 1
;;

exception Done

module Options =
struct
  let verbose = ref false
  let solution = ref false
  let compare = ref false
end

let usage = Printf.sprintf "usage: %s [--options] file1 file2 [file3]" Sys.argv.(0)

let options =
  [
   ("--verbose",  Arg.Set  Options.verbose,  "print propositional solver diagnostic");
   ("--solution",  Arg.Set  Options.solution,  "consider file2 as a solution to the cudf problem in file1 and print the complete list of installed packages");
   ("--compare", Arg.Set Options.compare, "compare solutions file2 file3")
  ]

type status = 
  |Erased
  |Upgraded of Cudf.package
  |Downgraded of Cudf.package
  |Installed
  |Removed
  |Unchanged
  |Replaced of package

let main () =
  let input_files = ref [] in

  let _ =
    try Arg.parse options (fun f -> input_files := f::!input_files) usage
    with Arg.Bad s -> failwith s
  in

  let provides u vpkg =
    List.map (fun (pkg,v) -> pkg ) (Cudf.who_provides u vpkg)
  in
  let diff u1 u2 =
    let replaced = ref 0 in
    let removed = ref 0 in
    let downgraded = ref 0 in
    let updated = ref 0 in
    let newinst = ref 0 in

    let h = Hashtbl.create (Cudf.universe_size u1) in
    let _ =
      Cudf.iter_packages (fun pkg ->
        if pkg.installed then
          let pkgl = Cudf.lookup_packages u2 pkg.package in
          let prvl = List.flatten (List.map (fun vpkg ->
            provides u2 (vpkg :> Cudf_types.vpkg) 
            ) pkg.provides)
          in
          match pkgl,prvl with
          |[],[] -> Hashtbl.add h pkg Erased
          |[],l ->
              List.iter (fun p ->
                if p.installed then
                  (incr replaced ; Hashtbl.add h pkg (Replaced p))
              ) l
          |l,_ ->
              List.iter (fun p ->
                if p.installed then
                  if Cudf.version_matches p.version (Some(`Eq,pkg.version))
                  then Hashtbl.add h pkg Unchanged
                  else if Cudf.version_matches p.version (Some(`Gt,pkg.version))
                  then (incr updated ; Hashtbl.add h pkg (Upgraded p))
                  else if Cudf.version_matches p.version (Some(`Lt,pkg.version))
                  then (incr downgraded ; Hashtbl.add h pkg (Downgraded p))
                else
                  (incr removed ; Hashtbl.add h pkg Removed)
            ) l
      ) u1
    in
    let _ =
      Cudf.iter_packages (fun pkg ->
        if pkg.installed && ((Cudf.get_installed u1 pkg.package) = []) then
          (incr newinst ; Hashtbl.add h pkg Installed)
      ) u2
    in
    let number pkg = 
      try Cudf.lookup_package_property pkg "number"
      with Not_found -> string_of_int pkg.version
    in
    if !Options.solution then begin
      let a = Hashtbl.create (Hashtbl.length h) in
      Hashtbl.iter (fun pkg v -> 
        if Hashtbl.mem a pkg then ()
        else begin
          Hashtbl.add a pkg ();
          match v with
          |Upgraded p -> print_endline (Cudf_printer.string_of_package p)
          |Replaced p -> print_endline (Cudf_printer.string_of_package p)
          |Downgraded p -> print_endline (Cudf_printer.string_of_package p)
          |Installed -> print_endline (Cudf_printer.string_of_package pkg)
          |Unchanged -> print_endline (Cudf_printer.string_of_package pkg)
          |_ -> ()
        end
      ) h
    end
    ;
    if !Options.verbose || not !Options.solution then begin
      Hashtbl.iter (fun pkg -> function
        |Erased ->
            Printf.printf "Package %s Erased (not present in u2)\n" pkg.package
        |Removed ->
            Printf.printf "Package (%s,%s) Removed\n" pkg.package (number pkg)
        |Replaced p ->
            Printf.printf "Package (%s,%s) is replaced by (%s,%s)\n"
            pkg.package (number pkg) p.package (number p)
        |Upgraded p ->
            Printf.printf "Package %s upgraded from %s to %s\n"
            pkg.package (number pkg) (number p)
        |Downgraded p ->
            Printf.printf "Package %s downgraded from %s to %s\n"
            pkg.package (number pkg) (number p)
        |Installed -> 
            Printf.printf "Package (%s,%s) was freshly installed\n" pkg.package
            (number pkg)
        |Unchanged -> ()
      ) h
      ;
      Printf.printf
      "\nremoved = %d\nreplaced = %d\nupdated = %d\ndowngraded = %d\nnew = %d\n"
      !removed !replaced !updated !downgraded !newinst;
    end
    ;
    (!removed,!replaced,!updated,!downgraded,!newinst)
  in

  let parse_univ f1 =
    match parse_cudf f1 with
    |_,_,None -> 
        (Printf.eprintf "file %s is not a valid cudf document\n" f1 ; exit 1)
    |_,u,Some r -> u,r
  in

  let check_sol u r s =
    match Cudf_checker.is_solution (u,r) s with
    |false,reasonlist -> 
        (List.iter (fun r ->
          Printf.eprintf "%s\n" (Cudf_checker.explain_reason r)
        ) reasonlist;
        false)
    |true,_ -> true
  in

  let rec compare = function
    |[] -> 0
    |`Min(x,y)::l when x = y -> compare l
    |`Max(x,y)::l when x = y -> compare l
    |`Min(x,y)::_ -> if x < y then 1 else -1
    |`Max(x,y)::_ -> if x > y then 1 else -1
    |_ -> assert false
  in

  match !input_files with
  |[f3;f2;f1] when !Options.compare ->
      let (univ,req) = parse_univ f1 in
      let (_,sol1,_) = parse_cudf f2 in
      let (_,sol2,_) = parse_cudf f3 in
      begin match check_sol univ req sol1, check_sol univ req sol2 with
      |true,false -> (Printf.printf "%s is a valid solution but %s is not\n" f2 f3; exit 1)
      |false,true -> (Printf.printf "%s is a valid solution but %s is not\n" f3 f2; exit 1)
      |false,false -> (Printf.printf "Neither solutions are valid\n" ; exit 1)
      |true,true ->
          begin
            let (removed1,replaced1,updated1,downgraded1,newinst1) = diff univ sol1 in
            let (removed2,replaced2,updated2,downgraded2,newinst2) = diff univ sol2 in
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
          end
      end
  |[f2;f1] when !Options.solution ->
      let (univ,req) = parse_univ f1 in
      let (_,sol,_) = parse_cudf f2 in
      if check_sol univ req sol then
        ignore(diff univ sol)
      else exit 1
  |[f2;f1] ->
      let (_,univ1,_) = parse_cudf f1 in
      let (_,univ2,_) = parse_cudf f2 in
      ignore(diff univ1 univ2)
  |_ -> (Printf.eprintf "wrong number of parameters\n" ; exit 1)
;;

main ();;
