

open ExtLib
open Debian
open Common

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

  let pkglist = Debian.Packages.input_raw [List.hd !files] in
  let srclist arch =
    let l = Debian.Sources.input_raw (List.tl !files) in
    let conflicts l = List.filter_map (fun (v,l) -> Some v) l
    in
    let depends ll =
      List.map (fun l -> List.filter_map (fun (v,l) -> Some v) l) ll
    in
    List.filter_map (fun pkg ->
      let archs = pkg.Debian.Sources.architecture in
      if List.mem "all" archs || List.mem arch archs then
        Some (
        { Debian.Packages.default_package with
          Debian.Packages.name = pkg.Debian.Sources.name ^ "__source";
          depends = depends pkg.Debian.Sources.build_depends;
          conflicts = conflicts pkg.Debian.Sources.build_conflicts;
        }
        )
      else None
    ) l
  in

  let source = srclist !Options.arch in
  let _ = Debcudf.init_tables (source @ pkglist) in
 
  let sl = List.map (fun pkg -> Debcudf.tocudf pkg) source in
  let universe =
    let l = List.map (fun pkg -> Debcudf.tocudf pkg) pkglist in
    Cudf.load_universe (sl @ l)
  in
 
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
  Printf.eprintf "Broken Packages: %d\n%!" !i
;;

main () ;;
