
open IprLib
open ExtLib

open Common
open Db
open Db.Idbr

module Options =
struct
  let plain = ref false
  let installed_file = ref ""
  let outdir = ref ""
end

let usage = Printf.sprintf "usage: %s [-options] query" (Sys.argv.(0))
let options =
  [
    ("--plain", Arg.Set Options.plain,
    "Do not preserve debian semantic.  Creates a (possibly) unconsistent cudf document.");
    ("--installed", Arg.String (fun l -> Options.installed_file := l),
    "Get the installed packages from a file");
    ("--outdir", Arg.String (fun l -> Options.outdir := l),
    "Specify the results directory");
  ]

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

  let l = 
    match Input.parse_uri !uri with
    |(("pgsql"|"sqlite") as dbtype,info,(Some query)) -> begin
      Backend.init_database dbtype info (Idbr.parse_query query) ;
      let l = Backend.load_selection (`All) in
      Debian.Debcudf.init_tables l ;
      List.map Debian.Debcudf.tocudf l
    end
    |("deb",(_,_,_,_,file),_) -> begin
      let l = Debian.Parse.input_raw [file] in
      List.map Debian.Debcudf.tocudf l
    end
    |_ -> failwith "Not supported"
  in

  let oc =
    if !Options.outdir <> "" then begin
      let dirname = !Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 777 ;
      open_out (Filename.concat dirname ("res.cudf"))
    end else stdout
  in
  List.iter (fun pkg ->
    Printf.fprintf oc "%s\n" (Cudf_printer.string_of_package pkg)
  ) l
;;

main ();;

