
open IprLib

type solver

(** initialize the solver and indexes *)
val init : ?buffer:bool -> Cudf.universe -> solver

(** Low-level : run the solver to satisfy the given request *)
val solve : solver -> Diagnostic.request -> Diagnostic.diagnosis

(** check if the given package can be installed in the universe *)
val edos_install : solver -> Cudf.package -> Diagnostic.diagnosis

(** check if the give package list can be installed in the universe *)
val edos_coinstall : solver -> Cudf.package list -> Diagnostic.diagnosis

(** check if all packages in the Cudf.universe can be installed 
    @param callback : execute a function for each package 
    @return the number of broken packages
*)
val distribcheck : ?callback:(Diagnostic.diagnosis -> unit) -> solver -> int

(** compute the dependencies closure (cone) of the give package *)
val dependency_closure : Cudf.universe -> Cudf.package list -> Cudf.package list

val dump : solver -> string
