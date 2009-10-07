(* copyright (c) 2009 Jaap Boender & the MANCOOSI project *)

(* attempt at computation of strong conflicts with libmancoosi *)

open Common
open ExtLib
open Cudf
open Cudf_types

module Graph = Defaultgraphs.SyntacticDependencyGraph
module G = Graph.G
module V = Graph.PkgV
module E = Graph.PkgE

let usage = Printf.sprintf "usage: %s [--debug] [--log file] uri" Sys.argv.(0);;
let logfile = ref (open_out "/dev/null");;

let options = [
  ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
  ("--log", Arg.String (fun s -> close_out !logfile; logfile := open_out s), "Dump log information in file");
];;

let log s = 
begin
  output_string !logfile s
end;;

let preceding_packages gr p =
begin
  G.fold_pred_e (fun (_, dt, v) l ->
    match dt with
    | E.DirDepends | E.OrDepends ->
      begin
        match v with
        | V.Pkg p' -> p'::l
        | V.Or _ -> G.fold_pred (fun v' l' ->
          match v' with
          | V.Pkg p'' -> p''::l'
          | V.Or _ -> failwith "Inconsistent dependency graph (Or -> Or)"
        ) gr v l
      end
    | _ -> l
   ) gr p []
end;;

type conflict_type = Conjunctive | Other;;
let nr_conj_pairs = ref 0;;
let nr_other_pairs = ref 0;;

let add_pair pair_ht x y ct =
let (c1, c2) = if x < y then (x, y) else (y, x) in
begin
  try
    let c1_ht = Hashtbl.find pair_ht c1 in
    if not (Hashtbl.mem c1_ht c2) then
    begin
      Hashtbl.add c1_ht c2 ct;
      match ct with
      | Conjunctive -> incr nr_conj_pairs
      | Other -> incr nr_other_pairs
    end
  with Not_found ->
  begin
    let c1_ht = Hashtbl.create 128 in
    Hashtbl.add c1_ht c2 ct;
    Hashtbl.add pair_ht c1 c1_ht;
    match ct with
    | Conjunctive -> incr nr_conj_pairs
    | Other -> incr nr_other_pairs
  end
end;;

let add_predecessors gr c_pred_ht pred_ht c =
  let visited = Hashtbl.create 64 in
  let rec visit_conj acc = function
    [] -> acc
  | p::r ->
    if Hashtbl.mem visited p then visit_conj acc r
    else
    begin
      Hashtbl.add visited p true;
      visit_conj (p::acc)
        (G.fold_pred_e (fun (_,ct,p) l ->
          match ct with
          | E.DirDepends ->
            begin
              match p with
              | V.Pkg p' -> p'::l
              | _ -> failwith "Inconsistent dependency graph (Or -> DirDepends)"
            end
          | _ -> l
        ) gr (V.Pkg p) r)
    end in
  let rec visit_disj acc = function
    [] -> acc
  | p::r ->
    if Hashtbl.mem visited p then visit_disj acc r
    else
    begin
      Hashtbl.add visited p true;
      visit_disj (p::acc) (preceding_packages gr (V.Pkg p)@r)
    end in
begin
  let c_pred = visit_conj [] [c] in
    Hashtbl.add c_pred_ht c c_pred;
    Hashtbl.add pred_ht c (visit_disj c_pred 
      (List.fold_left (fun l p -> l@preceding_packages gr (V.Pkg p)) [] c_pred))
end;;

let _ =
let uri = ref "" in
begin
  at_exit (fun () -> Util.dump Format.err_formatter);
  let _ =
    try Arg.parse options (fun f -> uri := f) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then
  begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  Printf.eprintf "Parsing...%!";
  (* let timer = Util.Timer.create "Parsing" in
  Util.Timer.start timer; *)
  let u = match Input.parse_uri !uri with
  | ("deb", (_,_,_,_,file),_) ->
    begin
      let l = Debian.Packages.input_raw [file] in
      Debian.Debcudf.load_universe l
    end
  | (s, _, _) -> failwith (Printf.sprintf "%s: not supported\n" s) in
  (* ignore (Util.Timer.stop timer ()); *)
  Printf.eprintf "done\n%!";

  let slv = Depsolver.load u in
  let u = Cudf.load_universe (Depsolver.trim (Depsolver.load u) (Cudf.get_packages u)) in
  Printf.eprintf "Trimmed: %d\n" (universe_size u);

  Printf.eprintf "Generating dependency graph...%!";
  let gr = Graph.dependency_graph u in
  Printf.eprintf "done\n%!";

  Printf.eprintf "Enumerating conflicts...%!";
  let timer = Util.Timer.create "Enumerating conflicts" in
  Util.Timer.start timer;
  let m = CudfAdd.build_maps u in
  let cl = Cudf.fold_packages (fun l p ->
    List.fold_left (fun l' c ->
      if c < p then
      begin
       log (Printf.sprintf "%s <-> %s \n" (string_of_pkgname p.package)
          (string_of_pkgname c.package)
        );
        (c,p)::l'
      end
      else
        l'
    ) l (m.CudfAdd.who_conflicts p)
  ) [] u in
  Printf.eprintf "done\n";
  ignore (Util.Timer.stop timer ());
  Printf.eprintf "Explicit conflicts found: %d\n%!" (List.length cl);

  Printf.eprintf "Removing triangles...%!";
  let timer = Util.Timer.create "Removing triangles" in
  Util.Timer.start timer;
  let clf = List.filter (fun (c1, c2) ->
    match preceding_packages gr (V.Pkg c1) with
    | [c1_pred] -> begin
        match preceding_packages gr (V.Pkg c2) with
        | [c2_pred] -> if c1_pred = c2_pred then
          begin
            log (Printf.sprintf "%s and %s share a unique predecessor.\n"
            (string_of_pkgname c1.package) (string_of_pkgname c2.package));
            not (List.exists (fun v ->
              match v with
              | V.Pkg _ -> false
              | V.Or _ -> let vs = G.succ gr v in
                begin
                  if vs = [V.Pkg c1; V.Pkg c2] || vs = [V.Pkg c2; V.Pkg c1]
                  then
                    (log "-> and it is OK\n"; true)
                  else
                    false
                end
            ) (G.succ gr (V.Pkg c1_pred)))
          end
          else true
        | _ -> true
      end
    | _ -> true
  ) cl in
  Printf.eprintf "done\n";
  ignore (Util.Timer.stop timer ());
  Printf.eprintf "Explicit conflicts left after triangle removal: %d\n%!" (List.length clf);

  Printf.eprintf "Exploding direct dependencies...%!";
  let timer = Util.Timer.create "Exploding direct dependencies" in
  Util.Timer.start timer;
  let pred_ht = Hashtbl.create (2 * List.length clf) in
  let c_pred_ht = Hashtbl.create (2 * List.length clf) in
  let pair_ht = Hashtbl.create 8192 in
  List.iter (fun (c1, c2) ->
    add_predecessors gr c_pred_ht pred_ht c1;
    add_predecessors gr c_pred_ht pred_ht c2;
    List.iter (fun c1p ->
      List.iter (fun c2p ->
        if c1p <> c2p then add_pair pair_ht c1p c2p Conjunctive
      ) (Hashtbl.find c_pred_ht c2)
    ) (Hashtbl.find c_pred_ht c1);
  ) clf;
  List.iter (fun (c1, c2) ->
    List.iter (fun c1p ->
      List.iter (fun c2p ->
        if c1p <> c2p then add_pair pair_ht c1p c2p Other
      ) (Hashtbl.find pred_ht c2)
    ) (Hashtbl.find pred_ht c1)
  ) clf;
  Printf.eprintf "done\n";
  ignore (Util.Timer.stop timer ());

  let p = Util.Progress.create "Checking non-conjunctive pairs" in
  Util.Progress.set_total p !nr_other_pairs;
  let timer = Util.Timer.create "Checking non-conjunctive pairs" in
  Util.Timer.start timer;
  Hashtbl.iter (fun c1 c1_ht ->
    Hashtbl.iter (fun c2 ct ->
      match ct with
      | Conjunctive -> Printf.printf "conjunctive: %s <-> %s\n%!" (string_of_pkgname c1.package) (string_of_pkgname c2.package) 
      | Other -> 
        let d = Depsolver.edos_coinstall slv [c1;c2] in
        begin
          Util.Progress.progress p;
          match d.Diagnostic.result with
          | Diagnostic.Failure _ -> Printf.printf "other: %s <-> %s\n%!" (string_of_pkgname c1.package) (string_of_pkgname c2.package)
          | _ -> () 
        end
    ) c1_ht
  ) pair_ht;
  ignore (Util.Timer.stop timer ());

end;;
