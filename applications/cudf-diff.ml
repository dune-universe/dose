
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

let pp_set ~inst fmt s =
  let install pkg = if pkg.Cudf.installed && inst then "*" else "" in
  let rec aux fmt s =
    if (Cudf_set.cardinal s) = 1 then
      let v = Cudf_set.min_elt s in
      Format.fprintf fmt "@,%d%s" 
      v.Cudf.version (install v)
    else begin
      let v = Cudf_set.min_elt s in
      Format.fprintf fmt "@,%d%s," v.Cudf.version (install v);
      aux fmt (Cudf_set.remove v s) 
    end
  in
  if Cudf_set.is_empty s then ()
  else Format.fprintf fmt "@[%a@]" aux s

let rec pp_list ?(sep="") pp_element fmt = function
  |[h] -> Format.fprintf fmt "%a" pp_element h
  |h::t ->
      Format.fprintf fmt "%a%s@,%a"
      pp_element h sep (pp_list ~sep pp_element) t
  |[] -> ()
;;

let pp_cell fmt cell = Format.fprintf fmt "%s" cell

let pp_header widths fmt header =
  let first_row = Array.map (fun x -> String.make (x + 1) ' ') widths in
  Array.iteri (fun j cell ->
    Format.pp_set_tab fmt ();
    for z=0 to (Buffer.length header.(j)) - 1 do cell.[z] <- Buffer.nth header.(j) z  done;
    Format.fprintf fmt "%s" cell
  ) first_row
;;

let pp_row pp_cell fmt row =
  Array.iteri (fun j cell ->
    Format.pp_print_tab fmt ();
    Format.fprintf fmt "%a" pp_cell cell
  ) row
;;

let pp_tables pp_row fmt (header,table) =
  (* we build with the largest length of each column of the 
   * table and header *)
  let widths = Array.create (Array.length table.(0)) 0 in
  Array.iter (fun row ->
    Array.iteri (fun j cell ->
      widths.(j) <- max (Buffer.length cell) widths.(j)
    ) row
  ) table;
  Array.iteri (fun j cell ->
    widths.(j) <- max (Buffer.length cell) widths.(j)
  ) header;

  (* open the table box *)
  Format.pp_open_tbox fmt ();

  (* print the header *)
  Format.fprintf fmt "%a" (pp_header widths) header;
  (* print the table *)
  Array.iter (pp_row fmt) table;

  (* close the box *)
  Format.pp_close_tbox fmt ();
;;

type t = U of Cudf_set.t | R of Cudf_set.t | I of Cudf_set.t

let pp_diff fmt (univ,hl) =
  let header = Array.init ((List.length hl) + 1) (fun _ -> Buffer.create 50) in
  let a_hl = Array.of_list hl in
  Format.bprintf header.(0) "package names";
  Array.iteri (fun i (solname,_) -> Format.bprintf header.(i+1) "%s" solname) a_hl;
  let names = pkg_names univ in
  let table = 
    Array.init (StringSet.cardinal names) (fun _ ->
      Array.init ((List.length hl) + 1) (fun _ -> Buffer.create 50)
    )
  in
  let i = ref 0 in
  StringSet.iter (fun pkgname ->
    let all = makeset (Cudf.lookup_packages univ pkgname) in
    Format.bprintf table.(!i).(0) "%s{%a}" pkgname (pp_set ~inst:true) all;
    for j = 0 to (List.length hl) - 1 do
      let (solname,(filename,h)) = a_hl.(j) in
      let s = Hashtbl.find h pkgname in
      let l =
        let l = ref [] in
        if not (Cudf_set.is_empty s.unchanged) then l := U(s.unchanged)::!l;
        if not (Cudf_set.is_empty s.removed) then l := R(s.removed)::!l ;
        if not (Cudf_set.is_empty s.installed) then l := I(s.installed)::!l ;
        !l
      in
      let pp_elem fmt = function
        |I s -> Format.fprintf fmt "I{%a}" (pp_set ~inst:false) s
        |U s -> Format.fprintf fmt "U{%a}" (pp_set ~inst:false) s
        |R s -> Format.fprintf fmt "R{%a}" (pp_set ~inst:false) s
      in
      Format.bprintf (table.(!i).(j+1)) "@[<h>%a@]" (pp_list ~sep:"," pp_elem) l  
    done;
    incr i;
  ) names;
  let pp_buffer fmt t = Format.fprintf fmt "%s" (Buffer.contents t) in 
  Format.fprintf fmt "@[%a@]" (pp_tables (pp_row pp_buffer)) (header,table)
;;

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
      Format.fprintf fmt "@[%a@]@." pp_diff (univ,sollist);
;;


main ();;
