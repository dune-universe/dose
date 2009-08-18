
(** print a cudf package.
    @param short : optionally print only <name,number> *)
val print_package : ?short : bool -> Cudf.package -> string

(** Failures reasons for sat solver *)
type reason =
  | Dependency of (Cudf.package * Cudf.package list)
  | EmptyDependency of (Cudf.package * Cudf_types.vpkg list) 
  | Conflict of (Cudf.package * Cudf.package)
  | Installed_alternatives of Cudf.package list
  | To_install of (Cudf_types.vpkg * Cudf.package list)
  | To_remove of (Cudf_types.vpkg * Cudf.package)
  | To_upgrade of (Cudf_types.vpkg * Cudf.package list)
  | To_upgrade_singleton of (Cudf_types.vpkg * Cudf.package list)

type result =
  | Success of (unit -> Cudf.package list) (** list of installed packages *)
  | Failure of (unit -> reason list)

(** Low-level : the type of request passed to the solver *)
type request =
  | Sng of Cudf.package (* test a single package *)
  | Lst of Cudf.package list (* test a list of packages *)
  | Req (* test a custom request *)

(** the result of the solver *)
type diagnosis = { request : request; result : result; }

(** print the result of the solver.
    @param explain : add a more verbose explanation of the failure or
    print the list of installed packages. *)
val print : ?explain:bool -> out_channel -> diagnosis -> unit

