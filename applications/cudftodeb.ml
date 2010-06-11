

open Cudf
open ExtLib
open Common

module Options = struct
  open OptParse

  let debug = StdOpt.store_true ()

  let description = "Report the broken packages in a package list"
  let options = OptParser.make ~description ()

  open OptParser
  add options ~short_name:'d' ~long_name:"debug" ~help:"Print debug information" debug;
end

let convert universe =
  let vr_RE = Pcre.regexp "(.*)--virtual" in
  let f1 (pkgname,constr) =
    if Pcre.pmatch ~rex:vr_RE pkgname then begin 
      let s = Pcre.exec ~rex: vr_RE pkgname in
      (Pcre.get_substring s 1, constr) 
    end
    else (pkgname,constr)
  in
  let f2 name (pkgname,constr) =
    not(Pcre.pmatch ~rex:vr_RE pkgname) && 
    not (name = pkgname && constr = None)
  in
  Cudf.fold_packages (fun (status,pkglist) p ->
    let p' = 
      {p with
      provides = List.map f1 p.provides;
      conflicts = List.filter (f2 p.package) p.conflicts;
      depends = List.map (List.filter (f2 p.package)) p.depends;
      pkg_extra =
        List.filter_map (function 
          |("recommends",`Vpkgformula l) ->
              begin match (List.map (List.filter (f2 p.package)) l) with
              |[] -> None
              |l -> Some ("recommends", `Vpkgformula l) end
          |("replaces",`Vpkglist l) ->
              begin match (List.filter (f2 p.package) l) with
              |[] -> None
              |l -> Some ("replaces", `Vpkglist l) end
          |(s,v) -> None
        ) p.pkg_extra }
    in
    if p.installed then (p'::status,pkglist) else (status,p'::pkglist)
  ) ([],[]) universe
;;

let main () =
  at_exit (fun () -> Util.dump Format.err_formatter);
  let posargs = OptParse.OptParser.parse_argv Options.options in
  if OptParse.Opt.get Options.debug then Boilerplate.enable_debug () ;

  let (status,pkglist,request) =
    match posargs with
    |[u] ->
        Common.Util.print_info "%s" u ;
        let (_,univ,req) = CudfAdd.load_cudf u in
        let (s,l) = convert univ in
        (s,l,req)
    |_ -> (Printf.eprintf "You must specify one cudf document\n" ; exit 1)
  in

  let status_oc = open_out "status" in
  let packages_oc = open_out "Packages" in
  let request_oc = open_out "Request" in
  let status_ofr = Format.formatter_of_out_channel status_oc in
  let packages_ofr = Format.formatter_of_out_channel packages_oc in
  let request_ofr = Format.formatter_of_out_channel request_oc in

  Cudf_printer.pp_packages packages_ofr pkglist;

  Cudf_printer.pp_packages status_ofr status;

  if request <> None then
    Cudf_printer.pp_request request_ofr (Option.get request);

  close_out status_oc ; close_out packages_oc ; close_out request_oc
;;

main ();;
