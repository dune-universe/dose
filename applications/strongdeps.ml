
open Nappe
open Cudf
open ExtLib
open Installer
open Graph

exception Done

module Options =
struct
  let confile = ref ""
  let debug = ref 0
  let dot = ref false
  let info = ref false
  let dump = ref false
  let strong_pred = ref false
  let src = ref ""
  let dst = ref ""

end

let usage = Printf.sprintf "usage: %s [-options] [cudf doc]" (Sys.argv.(0))
let options =
  [
   ("--confile",  Arg.String (fun l -> Options.confile := l ), "Specify a configuration file" );
   ("-d", Arg.Int (fun i -> Options.debug := i), "Turn on debugging info level");
   ("--dot", Arg.Set Options.dot, "Print the graph in dot format");
   ("--src",  Arg.String (fun l -> Options.src := l ), "Specify a list of packages to analyze" );
   ("--dst",  Arg.String (fun l -> Options.dst := l ), "Specify a pivot package" );
   ("--info", Arg.Set Options.info, "Print various aggregate information");
   ("--dump", Arg.Set Options.dump, "Dump the strong dependency graph in graph.marshal");
   ("--pred", Arg.Set Options.strong_pred, "Print strong predecessor (not direct)");
  ]

let input_file = ref ""
let file f =
  try
    match f with
    |s when Str.string_match (Str.regexp "^[\n\t ]*$") s 0 ->
        (print_endline usage ; exit 1)
    |_ -> input_file := f
  with _ -> (print_endline usage ; exit 1)

let and_sep_re = Str.regexp "\\s*;\\s*"
let pkg_re = Str.regexp "(\\([a-z][a-z0-9.+-]+\\)\\s*,\\s*\\([0-9][0-9]*\\))"
let parse_pkg s =
  let parse_aux str =
    if Str.string_match pkg_re str 0 then begin
      (Str.matched_group 1 str, int_of_string (Str.matched_group 2 str))
    end
    else
      (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Str.split and_sep_re s)

(* -------------------------------- *)

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

(* ----------------------------------- *)

module S = Set.Make(struct type t = int let compare = compare end)
let dependency_closure h root =
  let queue = Queue.create () in
  let visited = ref S.empty in
  Queue.add root queue;
  while (Queue.length queue > 0) do
    let pid = Queue.take queue in
    visited := S.add pid !visited;
    List.iter (fun l ->
      List.iter (fun p ->
        if not (S.mem p !visited) then
          Queue.add p queue;
      ) l
    ) (fst(Hashtbl.find h pid))
  done;
  S.elements !visited
;;

let complete_sd_graph fs fd h root =
  let queue = Queue.create () in
  let visited = ref S.empty in
  Queue.add (root,[root]) queue;
  while (Queue.length queue > 0) do
    let (pid,path) = Queue.take queue in
    visited := S.add pid !visited;
    List.iter (function 
      |[p2] ->
            if not (S.mem p2 !visited) then begin
              Queue.add (p2,pid::path) queue ;
              fd pid p2 ;
              List.iter(fun p1 -> fs p1 p2) (List.rev path)
            end
      |_ -> ()
    ) (fst(Hashtbl.find h pid))
  done
;;

(* ----------------------------------- *)

module PkgV = struct
    type t = int
    let compare = Pervasives.compare 
    let hash = Hashtbl.hash
    let equal l1 l2 = (l1 = l2)
end

module PkgE = struct
  type t = Strong | Direct | Disjunctive
  let compare _ _ = 0
  let default = Direct
end

module G = Imperative.Digraph.ConcreteLabeled(PkgV)(PkgE)

let transitive_reduction graph =
  G.iter_vertex (fun v ->
    List.iter (fun v' ->
      if v <> v' then
        List.iter (fun v'' ->
          if v' <> v'' then
            G.remove_edge graph v v''
        ) (G.succ graph v')
    ) (G.succ graph v);
  ) graph
;;

let dominator pr gr root = 
  let module T = Graph.Traverse.Dfs(G) in
  let dom = Hashtbl.create (G.nb_vertex gr) in
  G.iter_vertex (fun v -> Hashtbl.add dom v S.empty) gr ;
  let change = ref true in
  while !change do
    change := false ;
    T.postfix_component (fun n ->
      let newset =
        let pred = (G.pred gr n) in
        let rec inter = function
          |[h] -> S.singleton h
          |h::t -> S.inter (Hashtbl.find dom h) (inter t)
          |[] -> S.empty 
        in
        S.union (inter pred) (S.singleton n)
      in
      if not (S.equal newset (Hashtbl.find dom n)) then begin
        Hashtbl.replace dom n newset ;
        change := true
      end
    ) gr root
  done;
  (S.elements (Hashtbl.find dom root))
;;

(* ----------------------------------- *)

let print_debug lvl fmt =
  if !Options.debug = lvl then begin
    flush_all ();
    Printf.fprintf stdout fmt
  end
  else
    Printf.ifprintf stdout fmt
;;

let starttime = ref (Unix.gettimeofday ()) ;;
let start () =
  starttime := Unix.gettimeofday ()
;;
let stop ?(force=false) s =
  let pr = if force then Printf.printf else (print_debug 3) in
  pr "Time %s %.2f\n" s (Unix.gettimeofday () -. !starttime)
;;

(* ----------------------------------- *)

let memo_closure f size =
  let h = Hashtbl.create size in
  function p ->
    try Hashtbl.find h p
    with Not_found -> begin
      let r = f p in
      Hashtbl.add h p r;
      r
    end

let dependency_graph available =
  let gr = G.create () in
  List.iter (fun (pid,dl,cl) ->
    G.add_vertex gr pid ;
    List.iter (function
      |[p] -> 
          begin
            G.add_vertex gr p ;
            G.add_edge_e gr (G.E.create pid PkgE.Direct p)
          end
      |l ->
          List.iter (fun p ->
            G.add_vertex gr p ;
            G.add_edge_e gr (G.E.create pid PkgE.Disjunctive p)
          ) l
    ) dl
  ) available
  ;
  gr
;;

let sensitivity pr h f graph =
  let ht = Hashtbl.create (G.nb_vertex graph) in
  G.iter_vertex (fun v ->
    G.iter_succ (fun v' ->
      try Hashtbl.replace ht v' ((Hashtbl.find ht v') + 1) 
      with Not_found -> Hashtbl.add ht v' 1
    ) graph v
  ) graph;
  Hashtbl.iter (fun p _ ->
    if Hashtbl.mem ht p then
      Printf.printf "%d %s\n" (Hashtbl.find ht p) (pr p)
    else begin
      let dom = dominator pr (dependency_graph (f h p)) p in
      let d =
        List.filter (fun x -> 
          try ((Hashtbl.find ht x) >= 5000) && x <> p
          with Not_found -> false
        ) dom 
      in
      if d <> [] then begin
        let s = String.concat "," (List.map pr d) in
        Printf.printf "%s is dominated by %s \n" (pr p) s;
        flush_all ()
      end
    end
  ) h
;;

let strong_pred pr graph p =
  G.iter_pred_e (fun e ->
    if (G.E.label e) = PkgE.Strong then
      let pid = G.E.src e in
      let in_d = G.in_degree graph pid in
      Printf.printf "%s with In degree of %d\n" (pr pid) in_d 
  ) graph p
;;

let print_dom pr graph root =
  G.iter_vertex (fun pid ->
    let dom = dominator pr graph pid in
    let s = String.concat "," (List.map pr dom) in
    Printf.printf "%s dominance degree of %d : %s\n" (pr pid) ((List.length dom) -1) s
  ) graph
;;

(* ----------------------------------- *)

let main () =
  let _ =
    try Arg.parse options file usage
    with Arg.Bad s -> failwith s
  in
  let graph = G.create () in
  let universe = fst(parse_cudf !input_file) in
  let (get_pid,get_name,lookup,conflicts) = Installer.lookup universe in
  let exp = Installer.pkg_exp (lookup,conflicts,get_pid) in
  let availableHash = Hashtbl.create (2 * (Cudf.universe_size universe)) in
  let available =
    Cudf.fold_packages (fun l pkg ->
      let (id,dl,cl) = exp pkg in
      Hashtbl.add availableHash id (dl,cl) ;
      (id,dl,cl)::l
    ) [] universe
  in

  let memo_dc =
    memo_closure (dependency_closure availableHash) (Hashtbl.length availableHash)
  in

  let sort l = 
    let l' = 
      List.map (fun ((p,_,_) as pkg) ->
        let len = List.length (memo_dc p) in
        (len,pkg)
      ) l
    in
    let cmp (l1,_) (l2,_) = compare l2 l1 in
    List.map (fun (l,pkg) -> pkg) (List.sort ~cmp:cmp l')
  in

  let dependency_closure_exp h p1 = 
    List.map (fun p ->
      let (dl,cl) = Hashtbl.find h p in
      (p,dl,cl)
    ) (memo_dc p1)
  in

  let memo_dc_exp =
    memo_closure (dependency_closure_exp availableHash) (Hashtbl.length availableHash)
  in

  let print_package i =
    let (p,v) = get_name i in
    Printf.sprintf "(%s,%d)" p v
  in

  let print_info_solver problem p1 r instset dc =
    print_debug 5 "Package %s\n" (print_package p1) ;
    print_debug 5 "N. conflicts constraints %i\n" problem.conflicts ;
    print_debug 5 "N. dependencies constraints %i\n" problem.dependencies ;
    print_debug 5 "Problem Size %i\n" problem.size ;
    print_debug 5 "Installation Set Size %i\n" r.D.size ;
    print_debug 4 "Installation set %s : %s\n"
      (print_package p1)
      (String.concat ","
      (List.map (fun p ->
        Printf.sprintf "%s" (print_package p)
        ) instset
      ))
    ;
    print_debug 4 "Dependency Closure %s : %s\n"
      (print_package p1)
      (String.concat ","
      (List.map (fun (p,_,_) ->
        Printf.sprintf "%s" (print_package p)
        ) dc
      ))
    ;
    flush_all ()
  in

  let print_info (tot,o,i1,i2,sk) =
    Printf.eprintf "Analyzed %i packages out of %i\n" o tot ;
    Printf.eprintf "Skipped Packages %i\n" sk ;
    Printf.eprintf "Strong Dep Count %d\n" (G.nb_edges graph);
    Printf.eprintf "Strong Dep Graph Vertex %d\n" (G.nb_vertex graph);
    Printf.eprintf "N. of Calls to edos_install %i\n" i1 ;
    Printf.eprintf "N. of Calls to check_depends %i\n\n" i2 ;
    flush_all ();
  in

  let print_stats tot =
    let step = 20000 in
    let i = ref 0 in
    function (o,i1,i2,sk) ->
      if !i > step && (!Options.debug = 1) then begin
        print_info (tot,o,i1,i2,sk);
        i := 0
      end
      else incr i
  in

  let size = (List.length available) in
  let print_stats = print_stats size in
  let outer = ref 0 in
  let inner1 = ref 0 in
  let inner2 = ref 0 in
  let skipcount = ref 0 in

  let add ~label p1 p2 =
    if p1 <> p2 && not(G.mem_edge graph p1 p2) then begin
        G.add_edge_e graph (G.E.create p1 label p2) ;
        let t = if label = PkgE.Direct then "D" else "S" in
        print_debug 4 "%s --%s-> %s\n" (print_package p1) t (print_package p2)
    end else begin
        G.iter_succ (fun p ->
          if p <> p1 && not(G.mem_edge graph p1 p) then begin
            G.add_edge_e graph (G.E.create p1 (PkgE.Strong) p) ;
            print_debug 4 "%s --S-> %s\n" (print_package p1) (print_package p);
          end
        ) graph p2
    end
    ;
    print_stats (!outer,!inner1,!inner2,!skipcount); 
  in
  let add_strong = add ~label:PkgE.Strong in
  let add_direct = add ~label:PkgE.Direct in

  (* Main algorithm *)
  let strongdeps available =
    List.iter (fun (p1,dl,_) ->
      incr outer;
      G.add_vertex graph p1;

      if dl <> [] then begin
        incr inner1 ;
        let dc = memo_dc_exp p1 in
        let problem = Installer.init_solver dc in 
        let pb = Installer.copy_problem problem in
        (* let pb = Installer.init_solver dc in *)
        if (pb.conflicts = 0) && (pb.disjunctions = 0) then
          complete_sd_graph add_strong add_direct availableHash p1
        else begin
          let r = Installer.edos_install pb p1 in
          match r.D.result with
          |D.Failure(r) -> ()
          |D.Success(instset) -> begin
              print_info_solver pb p1 r instset dc; 
              complete_sd_graph add_strong add_direct availableHash p1; 
              List.iter (fun p2 ->
                if (p1 <> p2) && not(G.mem_edge graph p1 p2) then begin
                    incr inner2;

                    let pb = Installer.copy_problem problem in
                    (* let pb = Installer.init_solver dc in *)
                    if Installer.strong_depends pb p1 p2 then begin 
                      G.add_vertex graph p2 ;
                      G.add_edge_e graph (G.E.create p1 (PkgE.Strong) p2);
                      print_debug 4 "%s --S-> %s\n" (print_package p1) (print_package p2);
                      print_stats (!outer,!inner1,!inner2,!skipcount); 
                    end

                end
                else incr skipcount 
              ) instset
          end
        end
      end
      ;
    ) available
  in

  let pkg_src () =
    let l = ref [] in
    List.iter (fun (p,v) ->
      let pid = get_pid(p,v) in
      let (dl,cl) = Hashtbl.find availableHash pid in
      l := (pid,dl,cl)::!l
    ) (parse_pkg !Options.src)
    ;
    !l
  in
  let pkg_dst () =
    (* all packages q in R s.t. q is in the dependency closure of p *)
    let (p,v) = List.hd(parse_pkg !Options.dst) in
    let pid = get_pid(p,v) in
    List.filter_map (fun ((p,_,_) as pkg) ->
      if List.mem pid (memo_dc p) then
        Some(pkg)
      else None
    ) available
  in

  let pkg_src_list = ref [] in
  let pkg_dst_list = ref [] in
  let plist = 
    if !Options.src <> "" && !Options.dst <> "" then begin
      let (p,v) = List.hd(parse_pkg !Options.dst) in
      let pid = get_pid(p,v) in
      let (dl,cl) = Hashtbl.find availableHash pid in
      let dst = (pid,dl,cl) in
      pkg_src_list := pkg_src ();
      pkg_dst_list := [dst];
      (dst::!pkg_src_list)
    end
    else if !Options.src <> "" then begin
      pkg_src_list := pkg_src ();
      !pkg_src_list
    end
    else if !Options.dst <> "" then begin
      pkg_dst_list := pkg_dst ();
      !pkg_dst_list
    end
    else available
  in
  strongdeps (sort plist)
  ;

  if !Options.dump then begin
    let out_ch = open_out "graph.marshal" in
    Marshal.to_channel out_ch graph [] ;
    close_out out_ch
  end
  ;

  if !Options.info then begin
    print_info (size,!outer,!inner1,!inner2,!skipcount) ;
    if !Options.dst <> "" then begin
      let (p,v) = List.hd(parse_pkg !Options.dst) in
      let pid = get_pid(p,v) in
      let in_d = G.in_degree graph pid in
      Printf.printf "In degree for (%s,%d) : %d \n" p v in_d ;
    end
  end
  ;

  let module Display = 
    struct
      include G
      let vertex_name v = Printf.sprintf "\"%s\"" (print_package v)

      let graph_attributes = fun _ -> []
      let get_subgraph = fun _ -> None

      let default_edge_attributes = fun _ -> []
      let default_vertex_attributes = fun _ -> []

      let vertex_attributes v =
        if List.exists (fun (p,_,_) -> p = v) !pkg_src_list then
          [`Color 1]
        else if List.exists (fun (p,_,_) -> p = v) !pkg_dst_list then
          [`Color 10]
        else []

     let edge_attributes e =
        let t = 
          match G.E.label e with
          |PkgE.Strong -> [`Style `Bold]
          |PkgE.Direct -> [`Style `Dotted]
          |PkgE.Disjunctive -> [`Style `Dashed]
        in
        if (List.exists (fun (p,_,_) -> p = (G.E.src e)) !pkg_src_list) &&
           (List.exists (fun (p,_,_) -> p = (G.E.dst e)) !pkg_dst_list) then
          (`Color 5) :: t
        else t
    end
  in
  let module D = Graph.Graphviz.Dot(Display) in
  let module T = Graph.Traverse.Dfs(G) in
  let module I = Oper.I(G) in
  let sd_graph graph = 
    (* we create a new graph ignoring uninteresting edges/vertex, 
     * that is direct dependencies and vertex without in/out strong
     * (real) dependencies *)
    let gr = G.create () in
    G.iter_vertex (fun v ->
      let f v =
        let out_strong =
          List.exists (fun e ->
            (G.E.label e) = PkgE.Strong
          ) (G.succ_e graph v)
        in
        let in_strong =
          List.exists (fun e ->
            (G.E.label e ) = PkgE.Strong
          ) (G.succ_e graph v)
        in
        if (in_strong || out_strong) then begin
          G.add_vertex gr v;
          G.iter_succ_e (fun e ->
            G.add_edge_e gr e
          ) graph v
        end
      in
      T.prefix_component f graph v
    ) graph
    ;
    gr
  in

  if !Options.dot then begin
    let dg = dependency_graph plist in
    D.output_graph stdout (sd_graph (I.union dg graph)) 
  end
  else if (!Options.dst <> "") && !Options.strong_pred && not !Options.dot then
    let (p,v) = List.hd(parse_pkg !Options.dst) in
    let pid = get_pid(p,v) in
    strong_pred print_package graph pid
  else if not (!Options.info) then begin 
    sensitivity print_package availableHash dependency_closure_exp graph;
(*    print_newline ();
    let dg = dependency_graph plist in (* XXX plist where !! *)
    print_dom print_package dg ;
    *)
  end
;;

main ();;

(* ---------------Old Functions - Not Used -------------------- *)

let direct_dep h p1 p2 =
  try
    match Hashtbl.find h p1 with
    |[],_ -> false
    |ll,_ -> List.exists (List.mem p2) ll
  with Not_found -> assert false
;;

(* Dijkstra shortest path - modified *)
let find_path filter h v1 v2 =
  let visited = Hashtbl.create 97 in
  let heap = ref [] in
  let rec loop () =
    match !heap with
    |[] -> raise Not_found
    |(v,p)::_ when v = v2 -> List.rev p
    |(v,p)::tl ->
        begin
          heap := tl;
          if not (Hashtbl.mem visited v) then begin
            Hashtbl.add visited v ();
            List.iter (fun l ->
              List.iter (fun e ->
                if filter e then
                  heap := ((e, e :: p)::!heap)
              ) l
            ) (fst(Hashtbl.find h v))
          end;
          loop ()
        end
  in
  heap := (v1, [])::!heap;
  loop ()
;;


let rec add_sd_path f p = function
  |hd::tl when p = hd -> add_sd_path f hd tl
  |hd::tl -> f p hd ; add_sd_path f hd tl
  |[] -> ()
;;


let rec addlist f h p = function
  |[] -> ()
  |pid::t as l ->
      List.iter(fun pid -> if not (Hashtbl.mem h pid) then f(p,pid)) l ;
      if not (Hashtbl.mem h pid) then addlist f h pid t
;;


