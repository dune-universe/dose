

open ExtLib
open Debian
open Common
module Src = Debian.Sources
module Deb = Debian.Packages

module Options = struct
  let show_successes = ref true
  let show_failures = ref true
  let explain_results = ref false
  let output_xml= ref false
  let arch = ref ""
end

let usage = Printf.sprintf "usage: %s [-options] uri" Sys.argv.(0) ;;

let options = [
  ("--arch", Arg.String (fun s -> Options.arch := s), "Specify the architecture.");
  ("--explain", Arg.Set Options.explain_results, "Explain the results");
  ("--failures", Arg.Clear Options.show_successes, "Only show failures");
  ("--successes", Arg.Clear Options.show_failures, "Only show successes");
  ("--xml", Arg.Set Options.output_xml, "Output results in XML format");
  ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
];;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let files = ref [] in
  let _ =
    try Arg.parse options (fun f -> files := !files @ [f] ) usage
    with Arg.Bad s -> failwith s
  in
  if List.length !files < 2 then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  Printf.eprintf "Parsing and normalizing...%!" ;
  let pkglist = Deb.input_raw [List.hd !files] in
  let srclist = 
    let l = Src.input_raw (List.tl !files) in
    Src.sources2packages !Options.arch l 
  in
  let _ = Debcudf.init_tables (srclist @ pkglist) in
 
  let sl = List.map (fun pkg -> Debcudf.tocudf pkg) srclist in
  let l = List.fold_left (fun acc pkg -> (Debcudf.tocudf pkg)::acc) sl pkglist in
  let universe = Cudf.load_universe l in
  Printf.eprintf "done\n%!" ;

  Printf.eprintf "Init solver...%!" ;
  let solver = Depsolver.init universe in
  Printf.eprintf "done\n%!" ;

  let i = ref 0 in
  let result_printer = function
    |{Diagnostic.result = Diagnostic.Failure (_) } when !Options.show_successes -> incr i
    |{Diagnostic.result = Diagnostic.Failure (_) } as r -> (
        incr i;
        Diagnostic.print ~explain:!Options.explain_results stdout r )
    |r when !Options.show_failures -> ()
    |r -> Diagnostic.print ~explain:!Options.explain_results stdout r
  in

  Printf.eprintf "Solving...\n%!" ;

  List.iter (fun pkg ->
      let d = Depsolver.edos_install solver pkg in
      result_printer d
  ) sl
  ;
  Printf.eprintf "Broken Packages: %d\n" !i
;;

main () ;;
