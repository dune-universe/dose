(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib 
open Common

exception Done

module Options =
struct
  let cudf = ref false
  let verbose = ref false
  let dump = ref false
  let out = ref ""
end

let usage = Printf.sprintf "usage: %s [-options] [cudf doc]" Sys.argv.(0)

let options =
  [
   ("--verbose", Arg.Set Options.verbose, "");
   ("--dump", Arg.Set Options.dump, "propositional solver dump");
   ("--cudf", Arg.Set  Options.cudf, "print the cudf solution (if any)");
   ("--out", Arg.String (fun l -> Options.out := l),  "Specify the output file");
   ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
  ]

let main () =
  let input_file = ref "" in

  let _ =
    try Arg.parse options (fun f -> input_file := f) usage
    with Arg.Bad s -> failwith s
  in

  Printf.eprintf "parsing CUDF ...%!"; 
  let timer = Util.Timer.create "Parse" in
  Util.Timer.start timer;
  let (universe,request) = 
    match CudfAdd.load_cudf !input_file with
    |_,u,None -> 
        (Printf.eprintf "This cudf document does not contain a valid request\n" ; exit 1)
    |_,u,Some(r) -> u,r
  in
  Util.Timer.stop timer ();
  Printf.eprintf "done.\n%!"; 

  Printf.eprintf "Prepare ...%!";
  let timer = Util.Timer.create "Prepare" in
  Util.Timer.start timer;
  let problem = Cudfsolver.load universe request in
  Util.Timer.stop timer ();
  Printf.eprintf "done.\n%!"; 

  Printf.eprintf "Solve ...%!";
  let timer = Util.Timer.create "Solve" in
  Util.Timer.start timer;
  let result = Cudfsolver.solve problem in
  Util.Timer.stop timer ();
  Printf.eprintf "done.\n%!"; 

  match result with
  |{Diagnostic.result = Diagnostic.Success f } ->
      begin 
        if !Options.cudf then begin
          let oc = 
            if !Options.out <> "" then begin
              let fname = !Options.out in
              Printf.printf "cudf solution saved in %s\n" fname;
              open_out fname
            end else stdout
          in
          List.iter (fun pkg ->
            Printf.fprintf oc "%s\n" 
            (Cudf_printer.string_of_package 
            { pkg with Cudf.installed = true })
          ) (f ())
        end
        else
          Diagnostic.print stdout result
        ;
        exit(0)
      end
  |_ ->
      begin
        Diagnostic.print ~explain:true stdout result;
        exit(1)
      end
;;

main () ;;
