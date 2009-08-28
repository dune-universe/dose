
open ExtLib
open Debian
open Common

module Deb = Debian.Packages
module Src = Debian.Sources

(* module Options = struct end *)

let usage = Printf.sprintf "usage: %s [--options] candidates testing unstable sources" Sys.argv.(0) ;;

let options = [
  ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
];;

(* add a package only if it does not exist or it is a more recent version *)
let debianadd tbl x =
  try
    let y = Hashtbl.find tbl x.Deb.name in
    if (Debian.Version.compare y.Deb.version x.Deb.version) = -1 then
      (
        (*
        Printf.eprintf "replacing %s (= %s) with %s (= %s)\n%!" 
        x.Deb.name y.Deb.version y.Deb.name x.Deb.version ;
        *)
        Hashtbl.replace tbl x.Deb.name x
      )
  with Not_found -> 
    (
      (*
      Printf.eprintf "adding %s (= %s)\n%!" x.Deb.name x.Deb.version;
      *)
      Hashtbl.add tbl x.Deb.name x
    )
;;

let debunion u s =
  let h = Hashtbl.create (List.length u) in
  List.iter (fun p -> Hashtbl.add h p.Deb.name p) u;
  List.iter (debianadd h) s ;
  Hashtbl.fold (fun k v acc -> v::acc) h []
;;  

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let files = ref [] in
  let _ =
    try Arg.parse options (fun f -> files := !files @ [f] ) usage
    with Arg.Bad s -> failwith s
  in
  if List.length !files < 4 then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  let (candidates_f, testing_f, unstable_f, source_f) =
    match !files with
    |[c;t;u;s] -> (c,t,u,s)
    |_ -> assert false
  in

  Printf.eprintf "Read unstable %!" ;
  let unstable = 
    let l = Deb.input_raw [unstable_f] in
    let h = Hashtbl.create (List.length l) in
    List.iter (fun p -> Hashtbl.add h (p.Deb.name,Some p.Deb.version) p) l ;
    h
  in
  Printf.eprintf "(%d) done\n%!" Hashtbl.length unstable;

  Printf.eprintf "Read testing\n%!" ;
  let testing = Deb.input_raw [testing_f] in
  let testinghash = 
    let l = Deb.input_raw [testing_f] in
    let h = Hashtbl.create (List.length l) in
    List.iter (fun p -> Hashtbl.add h (p.Deb.name,Some p.Deb.version) p) l ;
    h
  in

  Printf.eprintf "Read sources\n%!" ;
  let source = 
    let l = Src.input_raw [source_f] in
    let h = Hashtbl.create (List.length l) in
    List.iter (fun p -> Hashtbl.add h (p.Src.name,p.Src.version) p) l ;
    h
  in

  Printf.eprintf "Read candidates\n%!" ;
  let candidates =
    let ch = open_in candidates_f in
    let l = ref [] in
    let rex = Str.regexp "[ \t]*:[ \t]*" in
    try while true do
      let line = input_line ch in
      if line = "" then ()
      else
        begin match Str.split rex line with
        |["Package";n] ->
            let line = input_line ch in
            if line = "" then ()
            else 
              begin match Str.split rex line with
              |["Version";v] -> l := (n,Some v)::!l
              |_ -> ()
            end
        |_ -> ()
      end
    done ; assert false 
    with End_of_file -> (close_in ch ; !l)
  in

  Printf.eprintf "Compute mcbin\n%!" ;
  (* binary migration candidates. *)
  let mcbin = List.filter_map (fun (n,v) -> 
    try 
      if Hashtbl.mem testinghash (n,v) then begin
         Printf.printf "Migration candidate %s (= %s) already in testing.  Ignoring\n%!" n (Option.get v);
        None
      end
      else
        Some (Hashtbl.find unstable (n,v));
    with Not_found -> begin
      Printf.printf "Migration candidate %s (= %s) not in unstable.  Ignoring\n%!" n (Option.get v);
      None
    end
    ) candidates
  in

  Printf.eprintf "Computing msbin\n%!" ;
  (* MS/bin = MC/bin \cap ( \bigcup_{p \in MC/bin} Inst (p, Testing + MC/bin) ) *)
  let unionl = debunion testing mcbin in
  let _ = Debian.Debcudf.init_tables unionl in
  let universe =
    let l = List.map (fun p ->
        let source = 
            let n,v = 
              match p.Deb.source with
              |"",_ -> (p.Deb.name,p.Deb.version)
              |n,None -> (n,p.Deb.version)
              |n,Some v -> (n,v)
            in
            [("Source",`String n) ; ("Sourceversion", `String v)]
          in
          let pkg = Debian.Debcudf.tocudf p in
          { pkg with Cudf.extra = source @ pkg.Cudf.extra }
      ) unionl
    in Cudf.load_universe l
  in

  let msbin =
    let mcbincudf =
      List.fold_left (fun acc p ->
        match Cudf.lookup_packages universe p.Deb.name with
        |[pkg] -> pkg::acc
        |_ -> assert false
      ) [] mcbin
    in
    let solver = Depsolver.init universe in
    (* XXX this can be faster if implemented as distrib check *)
    List.fold_left (fun acc p ->
      match Depsolver.edos_install solver p with
      |{Diagnostic.result = Diagnostic.Success fl} -> p::acc
      |{Diagnostic.result = Diagnostic.Failure (_) } as r -> begin
          Printf.printf "Source package %s cannot migrate because :\n%!"
          (Cudf.lookup_package_property p "Source") ;
          Diagnostic.print ~explain:true stdout r ;
          acc
      end
    ) [] mcbincudf
  in

  List.iter (fun p -> Printf.printf "%s\n%!" (Cudf_printer.string_of_package p)) msbin ;

  Printf.eprintf "Compute mssource\n%!" ;
  (* MS/source = { S | \forall p \in S, p \in MS/bin } *)
  let mssource = 
    let srchash = Hashtbl.create 200 in
    List.iter (fun pkg ->
      let binname = pkg.Cudf.package in
      let binver = Cudf.lookup_package_property pkg "Number" in
      let srcname = Cudf.lookup_package_property pkg "Source" in
      let srcver = Cudf.lookup_package_property pkg "Sourceversion" in
      try
        let binlist = Hashtbl.find srchash srcname in
        Hashtbl.replace srchash srcname (List.remove binlist (binname,None))
      with Not_found ->
        begin try
          let src = Hashtbl.find source (srcname,srcver) in
          let l = List.remove src.Src.binary (binname,None) in
          Hashtbl.add srchash srcname l
        with Not_found -> (
          Printf.printf
          "Package %s (= %s) is a migration candidate but I cannot find the corresponding source package %s (= %s). Ignoring.\n"
          binname binver srcname srcver
          )
        end
    ) msbin ;
    Hashtbl.fold (fun k v acc ->
      if v = [] then k::acc (* or all binaries are already in testing *)
      else 
        begin
        Printf.printf
        "Source package %s cannot migrate because the following packages are not migration candidates : %s \n" k 
        (String.concat "," (List.map (fun (n,_) -> n) v)) 
        ;
        acc
      end
    ) srchash []
  in

  List.iter (fun p -> Printf.printf "Source Package ready to migrate %s\n%!" p) mssource ; 
;;

main ();;

(*
Printf.eprintf "Binary Migration candidates \n%!" ;
List.iter (fun p -> Printf.eprintf "%s (= %s)\n%!" p.Deb.name p.Deb.version) mcbin ; 
*)

(*
Printf.eprintf "Compute mcsource\n%!" ;
(* MC/source = { S | \forall p \in S, p \in MC/bin } *)
let mcsource = 
  let srchash = Hashtbl.create 200 in
  List.iter (fun p ->
    let binname = p.Deb.name in
    let srcname = if fst(p.Deb.source) = "" then p.Deb.name else fst(p.Deb.source) in
    try
      let binlist = Hashtbl.find srchash srcname in
      Hashtbl.replace srchash srcname (List.remove binlist (binname,None))
    with Not_found ->
      begin try
        let src = Hashtbl.find source srcname in
        let l = List.remove src.Src.binary (binname,None) in
        Hashtbl.add srchash srcname l
      with Not_found -> failwith "Source package not found" end
  ) mcbin ;
  Hashtbl.fold (fun k v acc -> if v = [] then k::acc else acc) srchash []
in

List.iter (fun p -> Printf.printf "%s \n%!" p) mcsource ;
*)


