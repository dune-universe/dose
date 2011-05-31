(**************************************************************************)
(*  debcudf - Debian Packages file to CUDF conversion tool                *)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open Ocamlbuild_plugin

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

(* this lists all supported packages *)
let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
      run_and_read "ocamlfind list | cut -d' ' -f1"

(* this is supposed to list available syntaxes, but I don't know how to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

let _ = dispatch begin function
   | Before_options ->
       (* override default commands by ocamlfind ones *)
       Options.ocamlc     := ocamlfind & A"ocamlc";
       Options.ocamlopt   := ocamlfind & A"ocamlopt";
       Options.ocamldep   := ocamlfind & A"ocamldep";
       Options.ocamldoc   := ocamlfind & A"ocamldoc";
       Options.ocamlmktop := ocamlfind & A"ocamlmktop"

   | After_rules ->

       List.iter begin fun pkg ->
         flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
       end (find_packages ());

       (* Like -package but for extensions syntax. Morover -syntax is useless
        &* when linking. *)
       List.iter begin fun syntax ->
         flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
       end (find_syntaxes ());
  
       dep ["ocaml"; "compile"; "c_use_rpm" ] & ["rpm/dllrpm_stubs.so"];
       (*
       flag ["ocaml"; "link"; "c_use_rpm"; "byte"] & 
       S[
         A"-cclib"; A"-lrpm";
         A"-cclib"; A"-lrpmio"; 
         A"-ccopt"; A"-Lrpm";
         A"-dllib"; A"rpm/dllrpm_stubs.so"
       ];
       *)
(*
       flag ["ocaml"; "link"; "c_use_rpm" ; "byte"] & 
       S[
         A"-custom";
         A"-cclib"; A"-lrpm"; 
         A"-cclib"; A"-lrpmio"; 
         A"-dllpath"; A"rpm";
         A"-dllib"; A"-lrpm_stubs"
       ];
*)

      flag ["ocaml"; "link"; "c_use_rpm"; "native";] & S[
         A"-ccopt"; A"-Lrpm";
         A"-cclib"; A"-lrpm_stubs";
         A"-cclib"; A"-lrpm";
         A"-cclib"; A"-lrpmio"; 
       ]; 
       flag ["ocaml"; "link"; "c_use_rpm"; "byte";] & S[
         A"-ccopt"; A"-Lrpm";
         A"-cclib"; A"-lrpm_stubs";
         A"-cclib"; A"-lrpm";
         A"-cclib"; A"-lrpmio"; 
         A"-custom"
       ];
       flag ["ocaml"; "link"; "program"] & S[A"-linkpkg"];

       flag ["ocaml"; "pkg_threads"; "compile"] & S[A "-thread"];
       flag ["ocaml"; "pkg_threads"; "link"] & S[A "-thread"];

      
       (** Rule for native dynlinkable plugins *)
       rule ".cmxa.cmxs" ~prod:"%.cmxs" ~dep:"%.cmxa"
       (fun env _ ->
         let cmxs = Px (env "%.cmxs") and cmxa = P (env "%.cmxa") in
         Cmd (S [!Options.ocamlopt ; A"-shared" ; A"-o" ; cmxs ; cmxa])
       );

       (* for the moment we leave it this way, but this rule should be
        * specific to rpm.cm* *)
       flag ["ocamlmklib";] & S[ A"-lrpm"; A"-lrpmio"; ];



   | _ -> ()
end
