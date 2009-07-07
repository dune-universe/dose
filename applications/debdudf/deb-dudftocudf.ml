
open IprLib

open ExtLib
open Common
open Debian
open Ipr

module L = Xml.LazyList

module Options =
struct
  let outdir = ref ""
end

let usage = Printf.sprintf "usage: %s [-options] [debian dudf doc]" (Sys.argv.(0))
let options =
  [
    ("--outdir", Arg.String (fun l -> Options.outdir := l),
    "Specify the results directory");
  ]
let input_file = ref ""

(* ========================================= *)

module XmlDudf = struct 
  type dudfxml = {
    mutable timestamp : string ;
    mutable uid : string ;
    mutable installer : string ;
    mutable metaInstaller : string ;
    mutable problem : dudfproblem ;
  }
  and dudfproblem = {
    mutable packageStatus : string;
    mutable packageUniverse : (Debian.Parse.release * string) list;
    mutable action : string ;
    mutable desiderata : string ;
    mutable outcome : string
  }

  let dummydudf = {
    timestamp = "";
    uid = "";
    installer = "";
    metaInstaller = "";
    problem = { 
      packageStatus = "";
      packageUniverse = [];
      action = "";
      desiderata = "" ;
      outcome = ""
    };
  }
end

(* ========================================= *)

module AptPref = struct

  type criteria = {
    origin : string option;
    component : string option;
    release_version : string option;
    label : string option;
    archive : string option
  }

  let print_criteria = function
    { origin = o ;
      component = c ;
      release_version = v ;
      label = l ;
      archive = a
    } -> 
      let f = Option.may (Printf.eprintf "%s\n") in
      f o ; f c ; f v ; f l ; f a
  ;;

  type priority = int
  type generic = criteria
  type specific = {
    name : string ;
    version : string option ;
    criteria : criteria ;
  }

  type preferences = {
    target_release : string option ;
    specific : (specific * priority) list;
    generic  : (generic * priority) list
  }

  let map_criteria criteria = function
    |"v",v -> {criteria with release_version = Some v }
    |"c",v -> {criteria with component = Some v}
    |"o",v -> {criteria with origin = Some v}
    |"l",v -> {criteria with label = Some v}
    |"a",v -> {criteria with archive = Some v}
    |_,_ -> assert false

  let dummypref = { target_release = None ; specific = [] ; generic = [] }
  let dummycriteria = {
    origin = None ; component = None ; 
    release_version = None ; 
    label = None ; archive = None
  }
  let dummyspec = { name = "undef" ; version = None ; criteria = dummycriteria }

  let mapf preferences = function
    { Debian.Parse.Pref.package = pkg ; pin = pin ; pin_priority = priority } ->
      match pkg with
      |Debian.Parse.Pref.Star ->
          begin match pin with
          |Debian.Parse.Pref.Version _ -> assert false
          |Debian.Parse.Pref.Origin origin -> begin
              Printf.eprintf "Warning : origin is not currectly supported\n" ;
              let c = { dummycriteria with origin = Some origin } in
              {preferences with generic = (c, priority) :: preferences.generic}
          end
          |Debian.Parse.Pref.Release criteria -> 
              let c = List.fold_left map_criteria dummycriteria criteria in
              {preferences with generic = (c, priority) :: preferences.generic}
          end
      |Debian.Parse.Pref.Package name ->
          begin match pin with
          |Debian.Parse.Pref.Version version -> 
              let s = { dummyspec with name = name ; version = Some version } in
              {preferences with specific = (s, priority) :: preferences.specific }
          |Debian.Parse.Pref.Origin origin -> begin
              Printf.eprintf "Warning : origin is not currectly supported\n" ;
              let c = { dummycriteria with origin = Some origin } in
              let s = { dummyspec with name = name ; criteria = c} in
              {preferences with specific = (s, priority) :: preferences.specific}
          end
          |Debian.Parse.Pref.Release criteria ->
              let c = List.fold_left map_criteria dummycriteria criteria in
              let s = { dummyspec with name = name ; criteria = c } in
              {preferences with specific = (s, priority) :: preferences.specific}
          end
  ;;

  let parse ?tr s =
    let ch = IO.input_string s in
    let l = Debian.Parse.parse_preferences_in (fun x -> x) ch in
    let pref = List.fold_left mapf dummypref l in
    { pref with target_release = tr }

  let match_criteria constr c =
       (c.origin = None || c.origin = constr.origin) 
    && (c.release_version = None || c.release_version = constr.release_version) 
    && (c.component = None || c.component = constr.component)
    && (c.archive = None || c.archive = constr.archive)
    && (c.label = None || c.label = constr.label)
  ;;

  let match_version constr c = 
    if (Option.is_none constr) || (Option.is_none c) then false
    else
      let s = Str.global_replace (Str.regexp "\\*") "\\.*" (Option.get constr) in
      let version_re = Str.regexp s in
      Str.string_match version_re (Option.get c) 0
  ;;

  let match_specific constr c =
       (c.name = constr.name)
    && ((c.version = None) 
          || (c.version = constr.version)
          || (match_version c.version constr.version) )
    && (match_criteria constr.criteria c.criteria)
  ;;

  let find_specific constr l = 
    List.find_all (fun (c,_) -> match_specific constr c) l

  let find_generic constr l = 
    List.find_all (fun (c,_) -> match_criteria constr c) l

  let find_max l = List.fold_left (fun a (_,b) -> max a b) 0 l

  let get_priority pref info pkg =
    let number = Cudf.lookup_package_property pkg "number" in
    let constr = { dummyspec with name = pkg.Cudf.package ; version = Some number } in
    match (find_specific constr pref.specific, info) with
    |[], None -> None
    |[], Some info ->
        begin
          let constr = {dummycriteria with archive = Some(info.Debian.Parse.suite)} in
          match find_generic constr pref.generic with
          |[] -> None 
          |l -> Some(find_max l)
        end
    |l,_ -> Some(find_max l)

  let assign_priority preferences info package =
    match get_priority preferences info package with
    |None ->
      begin match preferences.target_release,info with
      |(Some _, None) | (None,_) ->
          if package.Ipr.Cudf.installed then 100 else 500
      |Some tr, Some info ->
          if package.Ipr.Cudf.installed then 100 else
          if not (tr = info.Debian.Parse.suite) then 500
          else 990
      end
    |Some p -> p

end

(* ========================================= *)

open XmlDudf

let main () =
  let _ =
    try Arg.parse options (fun f -> input_file := f) usage
    with Arg.Bad s -> failwith s
  in
  let xdata = XmlParser.parse_string (IO.read_all (Input.open_chan !input_file)) in
  let content_to_string node = Xml.fold (fun a x -> a^(Xml.to_string x)) "" node in
  let dudfproblem dudfprob node =
    Xml.fold (fun dudf node ->
      match Xml.tag node with
      |"package-status" -> {
        dudf with packageStatus = 
          Xml.fold (fun a n ->
            match Xml.tag n with
            |"installer" ->
                Xml.fold (fun _ n ->
                  match Xml.tag n with
                  |"status" -> Xml.fold (fun a x -> a^(Xml.cdata x)) "" n
                  |_ -> assert false
                ) "" n
            |"meta-installer" -> a
            |_ -> assert false
          ) "" node
      }
      |"package-universe" ->
        let (ul, rl) = 
          Xml.fold (fun (universe, release) n ->
            let filename = Xml.attrib n "filename" in
            match Xml.tag n with
            |"package-list" ->
                let e = (filename, Xml.fold (fun a x -> a^(Xml.cdata x)) "" n) in
                (e :: universe, release)
            |"package-release" ->
                let e = (filename, Xml.fold (fun a x -> a^(Xml.cdata x)) "" n) in
                (universe, e :: release)
            |_ -> assert false
          ) ([],[]) node
        in
        let fl = ref [] in
        let universe = 
          List.flatten (
            List.map (fun (relfn,c) ->
              let i = Str.search_backward (Str.regexp "_Release") relfn (String.length relfn) in
              let s = Str.string_before relfn i in
              let reldata = 
                let ch = IO.input_string c in
                let r = Debian.Parse.parse_release_in ch in
                let _ = IO.close_in ch in
                r
              in
              let cl =
                List.find_all (fun (fn,_) ->
                  Str.string_match (Str.regexp ("^"^s^".*_Packages$")) fn 0
                ) ul
              in
              List.map (fun (fn,c) -> fl := fn :: !fl ; (reldata,c)) cl
            ) rl
          )
        in
        let without_release = 
          List.map (fun (fn,c) ->
            Printf.eprintf "Warning : Package List without Release. %s\n" fn;
            (Debian.Parse.default_release,c)
          ) (List.find_all (fun (fn,_) -> not(List.mem fn !fl)) ul) ;
        in
        {dudf with packageUniverse = universe @ without_release}
      |"action" -> {dudf with action = content_to_string node}
      |"desiderata" -> {dudf with desiderata = content_to_string node}
      |"outcome" -> {dudf with outcome = content_to_string node}
      |s -> (Printf.eprintf "Warning : Unknown element %s\n" s ; dudf)
    ) dudfprob node
  in
  let dudfdoc dudfdoc node =
    Xml.fold (fun dudf node -> 
      match Xml.tag node with
      |"distribution" -> dudf
      |"timestamp" -> {dudf with timestamp = content_to_string node}
      |"uid" -> {dudf with uid = content_to_string node}
      |"installer" -> {dudf with installer = content_to_string node}
      |"meta-installer" -> {dudf with metaInstaller = content_to_string node}
      |"problem" -> {dudf with problem = dudfproblem dudf.problem node }
      |s -> (Printf.eprintf "Warning : Unknown elemenet %s\n" s ; dudf)
    ) dudfdoc node 
  in

  let id x = x in
  let dudf = dudfdoc dummydudf xdata in
  let preferences = AptPref.parse dudf.problem.desiderata in

  let infoH = Hashtbl.create 1031 in
  let all_packages =
    List.fold_left (fun acc (univinfo,contents) ->
      let ch = IO.input_string contents in
      let l = Debian.Parse.parse_packages_in id ch in
      let _ = IO.close_in ch in
      List.fold_left (fun s pkg -> 
        Hashtbl.add infoH (pkg.name,pkg.version) univinfo ;
        Ipr.Set.add pkg s
      ) acc l
    ) Ipr.Set.empty dudf.problem.packageUniverse
  in

  let installed_packages =
    let ch = IO.input_string dudf.problem.packageStatus in
    let l = Debian.Parse.parse_packages_in id ch in
    let _ = IO.close_in ch in
    List.fold_left (fun s pkg -> Ipr.Set.add pkg s) Ipr.Set.empty l
  in

  let l = Ipr.Set.elements (Ipr.Set.union all_packages installed_packages) in
  let _ = Debian.Debcudf.init_tables l in

  let installed =
    let h = Hashtbl.create 1031 in
    Ipr.Set.iter (fun pkg ->
      Hashtbl.add h (pkg.name,pkg.version) ()
    ) installed_packages
    ;
    h
  in

  let pl =
    List.map (fun pkg ->
      let inst = Hashtbl.mem installed (pkg.name,pkg.version) in
      let info = try Some(Hashtbl.find infoH (pkg.name,pkg.version)) with Not_found -> None in
      let cudfpkg = Debcudf.tocudf ~inst:inst pkg in
      let priority = AptPref.assign_priority preferences info cudfpkg in
      let cudfpkg = Ipr.add_extra ("priority", `Priority priority) cudfpkg in
      cudfpkg
    ) l
  in

  let universe = Cudf.load_universe pl in

  let request =
    let mapver = function
      |`Pkg p -> (p,None)
      |`PkgVer (p,v) -> begin
          try (p,Some(`Eq,Debcudf.get_version (p,v)))
          with Not_found -> failwith (Printf.sprintf "There is no version %s of package %s" p v)
      end
      |`PkgDst (p,d) ->
          try
            let l = Cudf.lookup_packages universe p in
            let pkg = List.find (fun pkg ->
                let number = Cudf.lookup_package_property pkg "number" in
                let info = Hashtbl.find infoH (pkg.Cudf.package,number) in
                info.Debian.Parse.suite = d
              ) l
            in
            let number = Cudf.lookup_package_property pkg "number" in
            (pkg.Cudf.package,Some(`Eq,Debcudf.get_version (pkg.Cudf.package,number)))
          with Not_found ->
            failwith (Printf.sprintf "There is no package %s in release %s " p d)
    in
    match Debian.Parse.parse_request_apt dudf.problem.action with
    |Debian.Parse.Upgrade (Some (suite))
    |Debian.Parse.DistUpgrade (Some (suite)) -> 
        let il = Set.fold (fun pkg acc -> `PkgDst (pkg.name,suite) :: acc) installed_packages [] in
        let l = List.map mapver il in
        { Cudf.problem_id = dudf.uid ; install = l ; remove = [] ; upgrade = [] }
    |Debian.Parse.Install l ->
        let l = List.map mapver l in
        { Cudf.problem_id = dudf.uid ; install = l ; remove = [] ; upgrade = [] } 
    |Debian.Parse.Remove l -> 
        let l = List.map (fun (`Pkg p) -> (p,None) ) l in
        { Cudf.problem_id = dudf.uid ; install = [] ; remove = l ; upgrade = [] }
    |Debian.Parse.Upgrade None -> 
        { Cudf.problem_id = dudf.uid ; install = [] ; remove = [] ; upgrade = [] }
    |Debian.Parse.DistUpgrade None -> 
        { Cudf.problem_id = dudf.uid ; install = [] ; remove = [] ; upgrade = [] }

  in

  let oc =
    if !Options.outdir <> "" then begin
      let dirname = !Options.outdir in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 777 ;
      open_out (Filename.concat dirname ("res.cudf"))
    end else stdout
  in
  Printf.fprintf oc "%s\n" (Cudf_printer.string_of_cudf (universe, request)) 
;;

main ();;

