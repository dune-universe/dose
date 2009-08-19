
open Debian
open Common
#ifdef HASDB
open Db
#endif

module Options = struct
  let show_successes = ref true
  let show_failures = ref true
  let explain_results = ref false
  let output_xml= ref false
end

let usage = Printf.sprintf "usage: %s [-options] uri" Sys.argv.(0) ;;

let options = [
  ("--explain", Arg.Set Options.explain_results, "Explain the results");
  ("--failures", Arg.Clear Options.show_successes, "Only show failures");
  ("--successes", Arg.Clear Options.show_failures, "Only show successes");
  ("--xml", Arg.Set Options.output_xml, "Output results in XML format");
  ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
];;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let uri = ref "" in
  let _ =
    try Arg.parse options (fun f -> uri := f ) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  Printf.eprintf "Parsing and normalizing...%!" ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let universe =
    match Input.parse_uri !uri with
#ifdef HASDB
    |(("pgsql"|"sqlite") as dbtype,info,(Some query)) -> begin
      Backend.init_database dbtype info (Idbr.parse_query query) ;
      let l = Backend.load_selection (`All) in
      Debian.Debcudf.load_universe l
    end
#endif
(*
    |("debsrc",(_,_,_,_,file),_) -> begin
      let l = Debian.Source.input_raw [file] in
      Debian.Debcudf.load_universe l
    end
*)
    |("deb",(_,_,_,_,file),_) -> begin
      let l = Debian.Packages.input_raw [file] in
      Debian.Debcudf.load_universe l
    end
    |("cudf",(_,_,_,_,file),_) -> begin
      let _, u, _ = CudfAdd.parse_cudf file in u
    end
    |_ -> failwith "Not supported"
  in
  ignore(Util.Timer.stop timer ());
  Printf.eprintf "done\n%!" ;
  Printf.eprintf "Init solver...%!" ;

  let timer = Util.Timer.create "Init solver" in
  Util.Timer.start timer;
  let solver = Depsolver.init universe in
  ignore(Util.Timer.stop timer ());

  let result_printer = function
    |{Diagnostic.result = Diagnostic.Failure (_) } when !Options.show_successes -> ()
    |{Diagnostic.result = Diagnostic.Failure (_) } as r ->
          Diagnostic.print ~explain:!Options.explain_results stdout r 
    |r when !Options.show_failures -> ()
    |r -> Diagnostic.print ~explain:!Options.explain_results stdout r
  in

  Printf.eprintf "done\n%!" ;
  Printf.eprintf "Solving...\n%!" ;
  let timer = Util.Timer.create "Solver" in
  Util.Timer.start timer;
  let i = Depsolver.distribcheck ~callback:result_printer solver in
  ignore(Util.Timer.stop timer ());
  Printf.eprintf "Broken Packages: %d\n%!" i
;;

main () ;;
