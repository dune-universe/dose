open ExtLib
open Common
open Cudf

module Options = struct
  open OptParse
  let description = "Do the Treinen Tests"
  let options = OptParser.make ~description
  include Boilerplate.MakeOptions(struct let options = options end)

  let out_file = StdOpt.str_option ()

  open OptParser
  add options ~long_name:"output" ~help:"Use output file" out_file;
end
module SD = Defaultgraphs.PackageGraph.G
module SC = Strongconflicts.CG

let debug fmt = Util.make_debug "StrongConflict" fmt
let info fmt = Util.make_info "StrongConflict" fmt
let warning fmt = Util.make_warning "StrongConflict" fmt

let oc = ref stdout;;

let prio_of x =
begin
  match x with
  | `String y ->
    if y = "required" then 1
    else if y = "important" then 2
    else if y = "standard" then 3
    else if y = "optional" then 4
    else if y = "extra" then 5
    else failwith (Printf.sprintf "Package with unknown priority (%s)" y)
  | _ -> failwith ("Package with wrong-typed priority")
end

let _ =
begin
  let posargs = OptParse.OptParser.parse_argv Options.options in
  let bars = [
    "Strongdeps_int.main";"Strongdeps_int.conj";
    "StrongDepGraph.transfrom.edges";"StrongDepGraph.transfrom.vertex";
    "Strongconflicts_int.local"; "Strongconflicts_int.seeding"
    ]
  in
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) bars;

  if OptParse.Opt.is_set Options.out_file then 
    oc := open_out (OptParse.Opt.get Options.out_file);

  let (universe,from_cudf,to_cudf) = Boilerplate.load_universe posargs in
  let universe = Depsolver.trim universe in
  let sd = Strongdeps.strongdeps_univ universe in
    info "Testing for priority-challenged dependencies...";
    SD.iter_edges (fun p q ->
      let p_prio = prio_of (List.assoc ("priority") p.pkg_extra)
      and q_prio = prio_of (List.assoc ("priority") q.pkg_extra) in
        if p_prio < q_prio then
          Printf.fprintf !oc "%s(%d) -> %s(%d)\n" p.package p_prio q.package q_prio
    ) sd;
  let sc = Strongconflicts.strongconflicts universe in
    info "Testing for conflicts between optional or lower packages...";
    SC.iter_edges (fun p q ->
      let p_prio = prio_of (List.assoc ("priority") p.pkg_extra)
      and q_prio = prio_of (List.assoc ("priority") q.pkg_extra) in
        if p_prio <= 4 && q_prio <= 4 then
          Printf.fprintf !oc "%s(%d) <-> %s(%d)\n" p.package p_prio q.package q_prio
    ) sc
end;;
