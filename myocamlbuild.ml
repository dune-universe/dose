
open Ocamlbuild_plugin;;

let modules_dirs = [
  "common";"deb";"eclipse";"rpm";"db";"algo";
  "doseparse"; "applications";"experimental"; "libcudf";
  "experimental/dudftocudf"
];;

let libraries = [
  "cudf"; "common";"debian";"eclipse";"rpm";"db";"algo";
  "boilerplate"; "boilerplateNoRpm";
];;

let doselibs = "doselibs" ;;

(* everybody can see the library dir *)
List.iter (fun d -> Pathname.define_context d [doselibs]) modules_dirs ;;

let _ = dispatch begin function
   | After_rules ->
       List.iter (fun lib ->
         flag ["ocaml"; "link"; "use_"^lib; "program"; "native"] & 
         S[A(doselibs^"/"^lib^".cmxa")];
         
         flag ["ocaml"; "link"; "use_"^lib; "program"; "byte"] & 
         S[A(doselibs^"/"^lib^".cma")]
       ) libraries
       ;

       flag ["ocaml"; "compile"] & S[A"-ccopt"; A"-O9"];

       dep ["ocaml"; "use_rpm5"; "compile"] & ["rpm/dllrpm5_stubs.so"];
       dep ["ocaml"; "use_rpm4"; "compile"] & ["rpm/dllrpm4_stubs.so"];

       flag ["ocaml"; "use_rpm"; "link"] & S[
         A"-cclib"; A"-lrpm";
         A"-cclib"; A"-lrpmio"; 
       ];
       flag ["ocaml"; "use_rpm5"; "link"; "library"] & S[
         A"-ccopt"; A"-Lrpm"; A"-cclib"; A"-lrpm5_stubs";
       ];
       flag ["ocaml"; "use_rpm4"; "link"; "library"] & S[
         A"-ccopt"; A"-Lrpm"; A"-cclib"; A"-lrpm4_stubs";
       ];
       (*
       flag ["ocaml"; "use_rpm"; "link"; "program"; "byte"] & S[ A"-custom" ];
       *)

       flag ["c"; "use_rpm"] & S[ A"-ccopt"; A"-I/usr/include/rpm"; A"-ccopt"; A"-O2" ];
       (*
       flag ["ocamlmklib"] & S[ A"-Lrpm"; A"-lrpm4_stubs"];
       *)

       flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
   | _ -> ()
end
