
module StringSet = Set.Make (String)

open ExtLib
open Common

module Cudf_set = CudfAdd.Cudf_set

type solution = {
  installed : Cudf_set.t ;
  removed : Cudf_set.t ;
  unchanged : Cudf_set.t
}

module Options = struct
  open OptParse
  let verbose = StdOpt.store_true ()

  let description = "Compare two or more solutions. Format : solvername:solutionfile"
  let options = OptParser.make ~description:description ()

  open OptParser
  add options ~short_name:'v' ~long_name:"verbose" ~help:"Verbose information" verbose;
end


let pkg_names univ =
  Cudf.fold_packages (fun names pkg ->
    StringSet.add pkg.Cudf.package names
  ) StringSet.empty univ

let makeset l = List.fold_right Cudf_set.add l Cudf_set.empty

(* the list of all packages (versions) that were installed before
 * but not now *)
let removed univ sol pkgname =
  let is_installed pkgname = Cudf.get_installed sol pkgname <> [] in
  let oldversions = Cudf.get_installed univ pkgname in
  if not (is_installed pkgname) && oldversions <> []
  then makeset oldversions
  else Cudf_set.empty

(* the list of all packages (versions) that were not installed before
 * but are installed now *)
let installed univ sol pkgname =
  let was_installed pkgname = Cudf.get_installed univ pkgname <> [] in
  let newversions = Cudf.get_installed sol pkgname in
  if not (was_installed pkgname) && newversions <> []
  then makeset newversions
  else Cudf_set.empty

(* for each pkgname I've the list of all versions that were installed, removed
 * or left unchanged *)
let diff univ sol =
  let pkgnames = pkg_names univ in
  let h = Hashtbl.create (StringSet.cardinal pkgnames) in
  StringSet.iter (fun pkgname ->
    let a = makeset (Cudf.lookup_packages univ pkgname) in
    let r = removed univ sol pkgname in
    let i = installed univ sol pkgname in
    let u = Cudf_set.diff a (Cudf_set.union r i) in
    let s = { removed = r ; installed = i ; unchanged = u } in
    Hashtbl.add h pkgname s
  ) pkgnames ;
  h

let pp_set fmt s =
  let rec aux fmt s = 
    if (Cudf_set.cardinal s) = 1 then
      Format.fprintf fmt "@,%d" (Cudf_set.min_elt s).Cudf.version
    else begin
      let v = Cudf_set.min_elt s in
      Format.fprintf fmt "@,%d," v.Cudf.version;
      aux fmt (Cudf_set.remove v s)
    end
  in
  if Cudf_set.is_empty s then ()
  else Format.fprintf fmt "@[<hv>%a@]" aux s

let pp_diff fmt (univ,hl) =
  StringSet.iter (fun pkgname ->
    let all = makeset (Cudf.lookup_packages univ pkgname) in
    Format.fprintf fmt "package | ";
    List.iter (fun (solname,_) ->
      Format.fprintf fmt " %s " solname
    ) hl ;
    Format.fprintf fmt "@]@.";
    Format.fprintf fmt "@[<hv>%s {%a} |" pkgname pp_set all;
    List.iter (fun (solname,(filename,h)) ->
      let s = Hashtbl.find h pkgname in
      if not (Cudf_set.is_empty s.installed) then
        Format.fprintf fmt "Installed{%a}@," pp_set s.installed;
      if not (Cudf_set.is_empty s.removed) then
        Format.fprintf fmt "Removed{%a}@," pp_set s.removed;
      if not (Cudf_set.is_empty s.unchanged) then
        Format.fprintf fmt "Unchanged{%a}@," pp_set s.unchanged;
    ) hl;
    Format.fprintf fmt "@]@.";
  ) (pkg_names univ)

let parse_univ f1 =
  match Boilerplate.load_cudf f1 with
  |_,_,None ->
      (Printf.eprintf "file %s is not a valid cudf document\n" f1 ; exit 1)
  |_,u,Some r -> u,r
;;

let check_sol u r s =
  match Cudf_checker.is_solution (u,r) s with
  |false,reasonlist ->
      (List.iter (fun r ->
        Printf.eprintf "%s\n" (Cudf_checker.explain_reason r)
      ) reasonlist;
      false)
  |true,_ -> true
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.verbose then Boilerplate.enable_debug () ;

  match posargs with
  |[] -> (Printf.eprintf "You must specify at least a universe and a solution\n" ; exit 1)
  |[u] -> (Printf.eprintf "You must specify at least a solution\n" ; exit 1)
  |u::l ->
      let (univ,req) = parse_univ u in
      let hl =
        List.filter_map (fun f ->
          let (h,f) =
            match Str.split (Str.regexp ":") f with
            |[f] -> (f,f)
            |[h;f] -> (h,f)
            |_ -> assert false
          in
          let (_,s,_) = Boilerplate.load_cudf f in
          if check_sol univ req s then Some (h,(f,s))
          else (Printf.eprintf "%s is not a valid solution. Discarded\n" f ; None)
        ) l
      in
      let sollist = List.map (fun (h,(f,s)) -> (h,(f,diff univ s))) hl in
      let fmt = Format.std_formatter in
      Format.fprintf fmt "%a" pp_diff (univ,sollist);
;;


main ();;
