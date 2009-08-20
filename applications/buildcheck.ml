

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
  let srclist arch =
    let l = Src.input_raw (List.tl !files) in
    (* as per policy, if the first arch restriction contains a !
     * then we assume that all archs on the lists are bang-ed.
     * cf: http://www.debian.org/doc/debian-policy/ch-relationships.html 7.1 *)
    let select = function
      |(v,(((false,_)::_) as al)) when List.for_all (fun (_,a) -> not(a = arch)) al -> Some v
      |(v,(((true,_)::_) as al)) when List.exists (fun (_,a) -> a = arch) al -> Some v
      |(v,[]) -> Some v
      |_ -> None
    in
    let conflicts l = List.filter_map select l in
    let depends ll = List.filter_map (fun l ->
      match List.filter_map select l with [] -> None | l -> Some l
      ) ll 
    in
    List.filter_map (fun pkg ->
      let archs = pkg.Src.architecture in
      if List.exists (fun a -> a = "all" || a = "any" || a = arch) archs then (
        Some (
        { Deb.default_package with
          Deb.name = "source---" ^ pkg.Src.name ;
          Deb.version = pkg.Src.version;
          depends = depends (pkg.Src.build_depends_indep @ pkg.Src.build_depends);
          conflicts = conflicts (pkg.Src.build_conflicts_indep @ pkg.Src.build_conflicts);
        }
        )
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
  Printf.eprintf "Broken Packages: %d\n" !i
;;

main () ;;
