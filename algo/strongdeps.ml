
module Make(Pr : Defaultgraphs.PrT) = struct

  module Default = Defaultgraphs.SimpleDepGraph(Pr)
  module PkgE = Default.PkgE
  module PkgV = Default.PkgV
  module G = Default.G
  module Pr = Default.Pr
  module S = Default.S
  open G

  let dependency_graph available =
    let gr = G.create () in
    List.iter (fun (pid,dl,cl) ->
      G.add_vertex gr pid ;
      List.iter (function
        |[p] -> G.add_vertex gr p 
        |l -> List.iter (fun p -> G.add_vertex gr p ) l
      ) dl
    ) available
    ;
    gr
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

  let memo_closure f size =
    let h = Hashtbl.create size in
    function p ->
      try Hashtbl.find h p
      with Not_found -> begin
        let r = f p in
        Hashtbl.add h p r;
        r
      end

  let strong_pred graph p =
    G.iter_pred_e (fun e ->
      if (G.E.label e) = PkgE.Strong then
        let pid = G.E.src e in
        let in_d = G.in_degree graph pid in
        Printf.printf "%s with In degree of %d\n" (Pr.pr pid) in_d 
    ) graph p


  let add ~label p1 p2 =
    if p1 <> p2 && not(G.mem_edge graph p1 p2) then begin
        G.add_edge_e graph (G.E.create p1 label p2) ;
    end else begin
        G.iter_succ (fun p ->
          if p <> p1 && not(G.mem_edge graph p1 p) then begin
            G.add_edge_e graph (G.E.create p1 (PkgE.Strong) p) ;
          end
        ) graph p2
    end
    ;
    print_stats (!outer,!inner1,!inner2,!skipcount);
  ;;

  let add_strong = add ~label:PkgE.Strong 
  let add_direct = add ~label:PkgE.Direct

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
  ;;

end
