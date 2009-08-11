
open IprLib

open Debian
open Common

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

let check universe =
  let solver = Depsolver.init universe in

  let result_printer = function
    |{Diagnostic.result = Diagnostic.Failure (_) } when !Options.show_successes -> ()
    |{Diagnostic.result = Diagnostic.Failure (_) } as r ->
          Diagnostic.print ~explain:!Options.explain_results stdout r 
    |r when !Options.show_failures -> ()
    |r -> Diagnostic.print ~explain:!Options.explain_results stdout r
  in
  let i = Depsolver.distribcheck ~callback:result_printer solver in
  Printf.eprintf "Broken Packages: %d\n%!" i
;;

(* add a package only if it does not exist or it is a more recent version *)
let debianadd tbl x =
  try 
    let y = Hashtbl.find tbl x.Ipr.name in
    if (Version.compare y.Ipr.version x.Ipr.version) = -1 then begin
      Hashtbl.remove tbl y.Ipr.name ;
      Hashtbl.add tbl x.Ipr.name x
    end
  with Not_found -> Hashtbl.add tbl x.Ipr.name x
;;

let init ps =
  Printf.eprintf "init cache (%d packages) ... %!" (Hashtbl.length ps);
  let ul = Hashtbl.fold (fun _ v acc -> v::acc) ps [] in
  Debian.Debcudf.init_tables ~reset:true ul ;
  let cache = Cudf.load_universe (List.map Debian.Debcudf.tocudf ul) in
  Printf.eprintf "done\n%!";
  cache
;;

let read ps ch =
  Printf.eprintf "read %!";
  let ll = Debian.Parse.parse_packages_in (debianadd ps) ch in
  Printf.eprintf "\n%d packages\n%!" (List.length ll);
  ps
;;

let check cache =
  Printf.eprintf "check packages ...%!";
  check cache ;
  Printf.eprintf "done \n%!";
;;

let main () =
  let uri = ref "" in
  let _ =
    try Arg.parse options (fun f -> uri := f ) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end ;

  let cmdfile = "/tmp/cmd-pipe" in
  let inputfile = "/tmp/input-pipe" in
  let mkfd file =
    try
      (try Unix.mkfifo file 0o600 with _ -> ());
      Unix.openfile file [Unix.O_RDONLY ; Unix.O_NONBLOCK] 0o600
    with Unix.Unix_error (e,s1,s2) ->
      failwith (Printf.sprintf "%s %s: %s" (Unix.error_message e) s1 s2)
  in
  let cmdfd = mkfd cmdfile in
  let cmdch = Unix.in_channel_of_descr cmdfd in
  let inputfd = mkfd inputfile in
  let inputch = IO.input_channel (Unix.in_channel_of_descr inputfd) in 

  let empty () = Hashtbl.create 30000 in
  let ps = ref (read (empty ()) (Input.open_file !uri)) in
  let undo = ref (empty ()) in
  let cache = ref (init (empty ())) in 

  try
    while true do
      match Unix.select [cmdfd] [] [] (-1.0) with
      | [fd],[],[] when fd = cmdfd ->
          begin try 
            match input_line cmdch with
            |"undo" -> ( ps := !undo ; undo := empty () )
            |"add" -> ( undo := Hashtbl.copy !ps ; ps := read !ps inputch )
            |"check" -> ( cache := init !ps ; check !cache )
            |_ -> Printf.eprintf "Command Not recognized\n%!"
          with End_of_file -> () end
      | _ -> ()
    done
  with
  | Unix.Unix_error (e,s1,s2) ->
      Printf.eprintf "%s %s: %s" (Unix.error_message e) s1 s2
  | e -> prerr_endline (Printexc.to_string e)

  ;

  Pervasives.at_exit (fun () -> 
    (try Unix.close cmdfd with _ -> ());
    (try Unix.unlink cmdfile with _ -> ());
    (try Unix.unlink inputfile with _ -> ());
    Util.dump Format.err_formatter
  );

;;

main () ;;
