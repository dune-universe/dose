
open Cudf
open ExtLib
open Common
#ifdef HASDB
open Db
#endif

exception Done

module Options =
struct
  let confile = ref ""
  let debug = ref 0
  let dot = ref false
  let boolean = ref false
  let info = ref false
  let strong_pred = ref false
  let src = ref ""
  let dst = ref ""
  let cone = ref ""

end

let usage = Printf.sprintf "usage: %s [-options] [cudf doc]" (Sys.argv.(0))
let options =
  [
   ("--confile",  Arg.String (fun l -> Options.confile := l ), "Specify a configuration file" );
   ("-d", Arg.Int (fun i -> Options.debug := i), "Turn on debugging info level");
   ("--dot", Arg.Set Options.dot, "Print the graph in dot format");
   ("--boolean", Arg.Set Options.boolean, "Output the boolean graph");
   ("--src",  Arg.String (fun l -> Options.src := l ), "Specify a list of packages to analyze" );
   ("--dst",  Arg.String (fun l -> Options.dst := l ), "Specify a pivot package" );
   ("--cone",  Arg.String (fun l -> Options.dst := l ), "Compute the dependency closure" );
   ("--info", Arg.Set Options.info, "Print various aggregate information");
   ("--pred", Arg.Set Options.strong_pred, "Print strong predecessor (not direct)");
  ]

let and_sep_re = Str.regexp "\\s*;\\s*"
let pkg_re = Str.regexp "(\\([a-z][a-z0-9.+-]+\\)\\s*,\\s*\\([0-9][0-9]*\\))"
let parse_pkg s =
  let parse_aux str =
    if Str.string_match pkg_re str 0 then begin
      (Str.matched_group 1 str, Str.matched_group 2 str)
    end
    else
      (Printf.eprintf "Parse error %s\n" str ; exit 1)
  in List.map parse_aux (Str.split and_sep_re s)

(* -------------------------------- *)

let parse_cudf doc =
  try
    let p = Cudf_parser.from_in_channel (open_in doc) in
    Cudf_parser.load p
  with
    Cudf_parser.Parse_error _
    | Cudf.Constraint_violation _ as exn ->
      Printf.eprintf "Error while loading CUDF from %s: %s\n%!"
      doc (Printexc.to_string exn);
      exit 1
;;

(* -------------------------------- *)

let main () =
  let uri = ref "" in
  let _ =
    try Arg.parse options (fun f -> uri := f ) usage
    with Arg.Bad s -> failwith s
  in
  if !uri == "" then begin
    Arg.usage options (usage ^ "\nNo input file specified");
    exit 2
  end;

  let versions = Hashtbl.create 1023 in
  let load_universe l = 
      Debian.Debcudf.init_tables l ;
      Cudf.load_universe (
        List.map (fun pkg ->
          let cudfpkg = Debian.Debcudf.tocudf pkg in
          Hashtbl.add versions 
          (pkg.Debian.Packages.name,pkg.Debian.Packages.version) cudfpkg ;
          cudfpkg
        ) l
      )
  in

  let universe =
    match Input.parse_uri !uri with
#ifdef HASDB
    |(("pgsql"|"sqlite") as dbtype,info,(Some query)) -> begin
      Backend.init_database dbtype info (Idbr.parse_query query) ;
      let l = Backend.load_selection (`All) in
      load_universe l
    end
#endif
    |("deb",(_,_,_,_,file),_) -> begin
      let l = Debian.Packages.input_raw [file] in
      load_universe l
    end
    |("cudf",(_,_,_,_,file),_) -> 
        let (_,u,_) = parse_cudf file in u
    |_ -> failwith "Not supported"
  in

  let get_cudfpkg (p,v) =
    try Hashtbl.find versions (p,v)
    with Not_found -> assert false
  in

  let print_package i = Diagnostic.print_package ~short:true i in

  let module Graph =
    Defaultgraphs.SyntacticDependencyGraph(struct let pr = print_package end)
  in

  let pkg_src () = List.map get_cudfpkg (parse_pkg !Options.src) in
  let pkg_dst () =
    (* all packages q in R s.t. q is in the dependency closure of p *)
    let (p,v) = List.hd(parse_pkg !Options.dst) in
    let pid = get_cudfpkg (p,v) in
    List.filter_map (fun pkg ->
      if List.mem pid (Depsolver.dependency_closure universe [pkg]) then
        Some(pkg)
      else None
    ) (Cudf.get_packages universe) 
  in
  let pkg_cone () =
    let (p,v) = List.hd(parse_pkg !Options.cone) in
    let pid = get_cudfpkg (p,v) in
    Depsolver.dependency_closure universe [pid]
  in

  let pkg_src_list = ref [] in
  let pkg_dst_list = ref [] in
  let plist =
    if !Options.src <> "" && !Options.dst <> "" then begin
      let (p,v) = List.hd(parse_pkg !Options.dst) in
      let pid = get_cudfpkg (p,v) in
      pkg_src_list := pkg_src ();
      pkg_dst_list := [pid];
      (pid::!pkg_src_list)
    end
    else if !Options.src <> "" then begin
      pkg_src_list := pkg_src ();
      !pkg_src_list
    end
    else if !Options.dst <> "" then begin
      pkg_dst_list := pkg_dst ();
      !pkg_dst_list
    end
    else if !Options.cone <> "" then 
      pkg_cone ()
    else Cudf.get_packages universe

  in
  Graph.D.output_graph stdout (Graph.dependency_graph (Cudf.load_universe plist))
;;

main ();;
