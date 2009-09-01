(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

open Cudf
open ExtLib
IFDEF HASDB THEN
open Db
END
open Common

exception Done

(* -------------------------------- *)
let pkg_re = Str.regexp "(\\([a-zA-Z][a-zA-Z\\-+_]+\\)\\s*,\\s*\\([0-9][0-9]+\\))"
let space_re = Str.regexp "[ \t]+" 
let and_sep_re = Str.regexp "\\s*,\\s*"
let or_sep_re = Str.regexp "\\s*|\\s*"
let comm_re = Str.regexp "^#.*$"
let eq_sep_re = Str.regexp "\\s*=\\s*"

let parse_aptlist s =
  let query_sep_RE = Str.regexp "\\s*\\/\\s*" in
  let l = List.mapi (fun i e -> match i with
      |0 -> `Arch e
      |1 -> `Suite e
      |2 -> `Comp e
      |3 -> `Date e
      |_ -> Printf.eprintf "parsing error %s\n" s ; exit 1
    ) (Str.split query_sep_RE s)
  in `And l

let parse_pkg s =
  let parse_aux str =
    if Str.string_match pkg_re str 0 then
      (Str.matched_group 0 str, int_of_string (Str.matched_group 1 str))
    else
      (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Str.split and_sep_re s)

let parse_task s = Str.split and_sep_re s


(* -------------------------------- *)

module Options =
struct
  let installR = ref 0
  let upgradeR = ref 0
  let removeR = ref 0
  let together = ref 1

  let universe = ref ""
  let status = ref ""

  let install = ref ""
  let remove = ref ""
  let upgrade = ref ""

  let upgradeAll = ref false

  let arch = ref ""
  let cudf = ref ""
  let task = ref ""

  let popcon = ref ""
  let outdir = ref ""
  let confile = ref ""
  let seed = ref 0
end

let progname = "randcudf"
let usage = Printf.sprintf "usage: %s [-options]" (Sys.argv.(0))

let options =
  [
   ("--installR", Arg.Int (fun l -> Options.installR := l ), "Create n Random Cudf with an install request" );
   ("--removeR",  Arg.Int (fun l -> Options.upgradeR := l ), "Create n Random Cudf with an remove request" );
   ("--upgradeR", Arg.Int (fun l -> Options.removeR := l ), "Create n Random Cudf with an upgrade request" );
   ("--together", Arg.Int (fun l -> Options.together := l ), "Install/remove/upgrade n random packages in groups of m" );

   ("--upgradeAll", Arg.Set  Options.upgradeAll, "Upgrade all installed packages");

   ("--universe", Arg.String (fun l -> Options.universe := l),  "");
   ("--status", Arg.String (fun l -> Options.status := l),  "");

   ("--install", Arg.String (fun l -> Options.install := l),  "Install the specified list of packages");
   ("--upgrade", Arg.String (fun l -> Options.upgrade := l),  "Remove the specified list of packages");
   ("--remove", Arg.String (fun l -> Options.remove := l),  "Upgrade the specified list of packages");

   ("--arch", Arg.String (fun l -> Options.arch := l),  "Specify the architecture"); 

   ("--task", Arg.String (fun l -> Options.task := l),  "Install packages from a task"); 
   ("--cudf", Arg.String (fun l -> Options.cudf := l),  "Load a cudf universe"); 

   ("--popcon", Arg.String (fun l -> Options.popcon := l),  "Use the popcon file to select random packages"); 
   ("--outdir", Arg.String (fun l -> Options.outdir := l),  "Specify the results directory"); 
   ("--confile",  Arg.String (fun l -> Options.confile := l ), "Specify a configuration file" );
   ("--seed", Arg.Int (fun l -> Options.seed := l ), "Set the pseudo-random generator seed" );
  ]

let parse_conf s =
  match Str.split eq_sep_re s with
  |["outdir";ss] -> Options.outdir := ss
  |["popcon";ss] -> Options.popcon := ss

  |["arch";ss] -> Options.arch := ss
(*  |["aptlist";ss] -> Options.aptlist := (parse_aptlist ss)::!Options.aptlist *)
(*  |["upgrade_aptlist";ss] -> Options.upgrade_aptlist := (parse_aptlist ss)::!Options.upgrade_aptlist *)
(*  |["basesystem";ss] -> Options.basesystem := Idbr.parse_query ss *)
  |["cudf";ss] -> Options.cudf := ss
  |["task";ss] -> Options.task := ss
  |["seed";ss] -> Options.seed := int_of_string ss
  |_ -> (Printf.eprintf "Parse error %s\n" s ; exit 1)


(* -------------------------------- *)

let rec read_file ?(max=0) parse filename =
  let ic = open_in filename in
  let n = ref 0 in
  let rec loop acc =
    let line =
      try 
        if (max <> 0) && !n >= max then None
        else (incr n ; Some (input_line ic))
      with End_of_file -> None
    in
    match line with
    | Some s when Str.string_match (Str.regexp "^#.*$") s 0 -> loop acc
    | Some s when Str.string_match (Str.regexp "^-*$") s 0 -> loop acc
    | Some s -> loop ((parse s)::acc)
    | None -> acc
  in
  loop []

let read_tasks task =
  if task = "" then []
  else 
    let filename = Printf.sprintf "tasks/%s.list" task in
    read_file Cudf_types.parse_vpkg filename

let read_basesytem system =
  if system = "" then []
  else read_file Cudf_types.parse_veqpkg system

let read_popcon max popcon =
  if popcon = "" then assert false
  else Array.of_list (read_file ~max:max Debian.Apt.parse_popcon popcon)

(* -------------------------------- *)

let parse_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    Cudf_parser.load p
  with
    Cudf_parser.Parse_error _
    | Cudf.Constraint_violation _ as exn ->
      Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
      doc (Printexc.to_string exn);
      exit 1
;;

(* -------------------------------- *)

let main () =
  let _ =
    try Arg.parse options (fun f -> ()) usage
    with Arg.Bad s -> failwith s
  in
  let _ = if !Options.confile <> "" then
    ignore(read_file parse_conf !Options.confile)
  in

  let _ =
    if !Options.seed <> 0 then Random.init !Options.seed
    else Random.self_init ()
  in

  let load_uri uri =
    match Input.parse_uri uri with
    |(("pgsql"|"sqlite") as dbtype,info,(Some query)) -> 
IFDEF HASDB THEN
      begin
        Backend.init_database dbtype info (Idbr.parse_query query) ;
        Backend.load_selection (`All) 
      end
ELSE
      failwith (dbtype ^ " Not supported")
END
    |("deb",(_,_,_,_,file),_) -> Debian.Packages.input_raw [file] 
    |(s,_,_) -> failwith (Printf.sprintf "%s not supported" s)
  in

  let (pkglist,universe) =
    match Input.parse_uri !Options.universe with
    |("cudf",(_,_,_,_,file),_) ->
        let _,u,_ = parse_cudf file in
        (Cudf.get_packages u,u)
    |_ ->
      let u = 
        if !Options.universe <> "" then load_uri !Options.universe 
        else failwith "universe required"
      in
      let s =
        if !Options.status <> "" then load_uri !Options.status
        else failwith "status required" 
      in
      let tables = Debian.Debcudf.init_tables (List.unique (u @ s)) in
      let u' = List.map (Debian.Debcudf.tocudf tables) u in
      let s' = List.map (Debian.Debcudf.tocudf tables ~inst:true) s in
      let pkglist = List.unique (u' @ s') in
      (pkglist, Cudf.load_universe pkglist)
  in

  let random_packages_popcon () =
    if !Options.popcon = "" then []
    else begin
      let max = 990 in
      let a = read_popcon 1000 !Options.popcon in
      let ll = ref [] in
      for i=0 to !Options.installR - 1 do
        let l = ref [] in
        for j=0 to !Options.together - 1 do
          let i = Random.int (max - 1) in
          let (_,name,_) = a.(i) in
          match Cudf.lookup_packages universe name with
          |[] -> ()
          |_ -> l := (name,None)::!l
        done;
        ll := !l::!ll
      done
      ;
      !ll
    end
  in

  let get_random pkglist n =
    let a = Array.of_list pkglist in
    let max = (Array.length a) in
    let l = ref [] in
    for i=0 to n - 1 do
      let j = Random.int (max - 1) in
      let pkg = a.(j) in
      l := (pkg.package,None)::!l
    done;
    !l
  in

  let random_packages pkglist n =
    let a = Array.of_list pkglist in
    let max = (Array.length a) in
    let ll = ref [] in
    for i=0 to n - 1 do
      let l = ref [] in
      for j=0 to !Options.together - 1 do
        let j = Random.int (max - 2) in
        let pkg = a.(j) in
        l := (pkg.package,None)::!l
      done;
      ll := !l::!ll
    done
    ;
    !ll
  in

  let installed () =
    Cudf.fold_packages (fun l pkg -> 
      if pkg.installed then pkg::l else l
    ) [] universe
  in

  let to_install_random () =
    if !Options.installR <> 0 then
      if !Options.popcon <> "" then random_packages_popcon ()
      else random_packages pkglist !Options.installR
    else []
  in

  let to_upgrade_random () = get_random (installed ()) !Options.upgradeR in 
  let to_remove_random () = get_random (installed ()) !Options.removeR in 

  let create_cudf (to_install,to_upgrade,to_remove) =
    let oc = 
      if !Options.outdir <> "" then begin
        let tmpfile = Filename.temp_file "rand" ".cudf" in
        let dirname = !Options.outdir in
        if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
        open_out (Filename.concat dirname (Filename.basename tmpfile)) 
      end else stdout
    in

    let request =
      { problem_id = "RAND-CUDF-GENERATOR";
        install = to_install ;
        upgrade = to_upgrade ;
        remove = to_remove
      }
    in

    Printf.fprintf oc "%s" (Cudf_printer.string_of_universe universe);
    Printf.fprintf oc "%s" (Cudf_printer.string_of_request request);
    close_out oc
  in 

  if !Options.installR <> 0 then
    List.iter (fun l -> create_cudf (l,[],[])) (to_install_random ()) ;

  if !Options.upgradeR <> 0 then
    create_cudf ([],to_upgrade_random (),[]) ;

  if !Options.removeR <> 0 then
    create_cudf ([],to_remove_random (),[]) ;

  if !Options.upgradeAll then
    create_cudf ([],List.map (fun pkg -> (pkg.package,None)) (installed ()),[])

;;

main ();;
