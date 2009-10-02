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
          | V.Or _ -> failwith "inconsistent dependency graph"
        ) gr v l
      end
    | _ -> l
   ) gr p []
end;;

let add_pair (l, r) =
begin
  Printf.eprintf "pair: %s, %s" (string_of_pkgname l.package) (string_of_pkgname r.package)
end

let rec explode_direct_dependencies gr c1_before c2_before c1 c2 =
(* c1/c2_before: list of packages already considered on left/right-hand side
 * c1/c2: list of packages to be considered on the left/righ5-hand side
 * Algorithm:
 * - find all conjunctive predecessors of packages in c1 [-> c1_pred]
 * - find all conjunctive predecessors of packages in c2 [-> c2_pred]
 * - add strong conflicts between every element of
 *   c1_pred and [c2 @ c2_before] 
 * - add strong conflicts between every element of
 *   c2_pred and [c1 @ c1_before]
 * - add strong conflicts between every element of c1_pred and c2_pred 
 * - recurse. *)
begin
  let c1work = c1 @ c1_before
  and c2work = c2 @ c2_before in
  let c1_pred =
    List.fold_left (fun l c1p ->
      if List.mem c1p c1work then
        l
      else
        List.fold_left (fun l' x ->
          match x with
          | V.Pkg x' -> x'::l'
          | _ -> l'
        ) l (G.pred gr (V.Pkg c1p))
    ) [] c1
  and c2_pred = 
    List.fold_left (fun l c1p ->
      if List.mem c1p c1work then
        l
      else
        List.fold_left (fun l' x ->
          match x with
          | V.Pkg x' -> x'::l'
          | _ -> l'
        ) l (G.pred gr (V.Pkg c1p))
    ) [] c2 in
    List.iter (fun l ->
      List.iter (fun r ->
        add_pair (l, r)
      ) (c2_pred @ c2work)
    ) c1_pred;
    List.iter (fun l ->
      List.iter (fun r ->
        add_pair (l, r)
      ) c1work
    ) c2_pred;
    explode_direct_dependencies gr c1work c2work c1_pred c2_pred
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
  let pl = match Input.parse_uri !uri with
  | ("deb", (_,_,_,_,file),_) ->
    begin
      let l = Debian.Packages.input_raw [file] in
      Debian.Debcudf.load_universe l
    end
  | (s, _, _) -> failwith (Printf.sprintf "%s: not supported\n" s) in
  (* ignore (Util.Timer.stop timer ()); *)
  Printf.eprintf "done\n%!";
  
  Printf.eprintf "Generating dependency graph...%!";
  let gr = Graph.dependency_graph (pl) in
  Printf.eprintf "done\n%!";

  Printf.eprintf "Enumerating conflicts...%!";
  let timer = Util.Timer.create "Enumerating conflicts" in
  Util.Timer.start timer;
  let m = CudfAdd.build_maps pl in
  let cl = Cudf.fold_packages (fun l p ->
    List.fold_left (fun l' c ->
      if c < p then
      begin
       log (Printf.sprintf "%s (%s) <-> %s (%s)\n" (string_of_pkgname p.package)
          (String.concat "," (List.map (fun x -> (string_of_pkgname x.package)) (preceding_packages gr (V.Pkg p))))
          (string_of_pkgname c.package)
          (String.concat "," (List.map (fun x -> (string_of_pkgname x.package)) (preceding_packages gr (V.Pkg c))))
        );
        (c,p)::l'
      end
      else
        l'
    ) l (m.CudfAdd.who_conflicts p)
  ) [] pl in
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
  List.iter (fun (c1, c2) ->
    explode_direct_dependencies gr [] [] [c1] [c2]
  ) clf;
  Printf.eprintf "done\n";
  ignore (Util.Timer.stop timer ());
end;;
