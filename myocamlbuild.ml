
open Ocamlbuild_plugin;;

let modules_dirs = [
  "common";"debian";"eclipse";"rpm";"db";"algo";
  "doseparse"; "applications";"experimental"; "libcudf"
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
       flag ["ocaml"; "use_rpm"; "link"; "native"] & S[
         A"-ccopt"; A"-Lrpm";
         A"-cclib"; A"-lrpm_stubs";
         A"-cclib"; A"-lrpm";
         A"-cclib"; A"-lrpmio";
       ];
       flag ["ocaml"; "link"; "use_rpm"; "byte"] & S[
         A"-ccopt"; A"-Lrpm";
         A"-cclib"; A"-lrpm_stubs";
         A"-cclib"; A"-lrpm";
         A"-cclib"; A"-lrpmio";
         A"-custom"
       ];
       dep ["ocaml"; "compile"; "use_rpm" ] & ["rpm/dllrpm_stubs.so"];
       flag ["ocamlmklib"] & S[ A"-lrpm"; A"-lrpmio"; ];
   | _ -> ()
end
