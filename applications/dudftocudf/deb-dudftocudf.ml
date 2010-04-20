
open ExtLib
open Common
open Debian

module Deb = Debian.Packages

module L = Xml.LazyList ;;

Random.self_init () ;;

module Options =
  struct
    let outdir = ref ""
    let problemid = ref ""
  end

let usage = Printf.sprintf "usage: %s [--options] [debian dudf doc]" (Sys.argv.(0))
let options =
  [
    ("--outdir", Arg.String (fun l -> Options.outdir := l), "Specify the results directory");
    ("--problemid", Arg.String (fun l -> Options.problemid := l), "Specify the problem identifier");
    ("--debug", Arg.Unit (fun () -> Util.set_verbosity Util.Summary), "Print debug information");
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
    mutable comment : dudfcomment ;
  }
  and dudfproblem = {
    mutable packageStatus : string;
    mutable packageUniverse : (Debian.Release.release * string) list;
    mutable action : string ;
    mutable desiderata : dudfdesiderata ;
    mutable outcome : dudfoutcome
  }
  and dudfoutcome = {
    result : string ;
    error : string
  }
  and dudfdesiderata = {
    aptpref : string
  }
  and dudfcomment = {
    user : string;
    tags : string list ;
    hostid : string
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
      desiderata = { aptpref = "" } ;
      outcome = { result = "" ; error = "" }
    };
    comment = { user = "" ; tags = [] ; hostid = "" }
  }
end

(* ========================================= *)

let _ = Curl.global_init Curl.CURLINIT_GLOBALALL ;;

let curlget buf url =
  let writer buf data = Buffer.add_string buf data ; String.length data in
  let code_from h =
    let res = Curl.getinfo h Curl.CURLINFO_RESPONSE_CODE in
    match res with
    |Curl.CURLINFO_Long(i) -> i
    |_ -> failwith "Expected long value for HTTP code"
  in
  let errorBuffer = ref "" in
  begin try
    let connection = Curl.init () in
    Curl.set_errorbuffer connection errorBuffer;
    Curl.set_writefunction connection (writer buf);
    Curl.set_followlocation connection true;
    Curl.set_url connection url;
    Curl.perform connection;
    begin match code_from connection with
    |200 -> begin
        Util.print_info "Download %s (time : %f)"
          (Curl.get_effectiveurl connection)
          (Curl.get_totaltime connection) ;
        Curl.cleanup connection
      end
    |code -> raise (Failure (Printf.sprintf "HTTP %d" code))
    end
  with
    |Curl.CurlException (reason, code, str) ->
        (Printf.eprintf "Error while downloading %s\n%s\n" url !errorBuffer;
         Curl.global_cleanup ();
         exit 1)
    |Failure s ->
        (Printf.eprintf "Caught exception while downloading %s\n%s\n" url s ;
         Curl.global_cleanup ();
         exit 1)
  end

let pkgget ?(bz=true) url =
  let buf = Buffer.create 10024 in
  curlget buf url;
  let s =
    if bz then
      Bz2.uncompress (Buffer.contents buf) 0 (Buffer.length buf)
    else
      Buffer.contents buf
  in
  Buffer.clear buf ;
  s
;;

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
    { Debian.Apt.Pref.package = pkg ; pin = pin ; pin_priority = priority } ->
      match pkg with
      |Debian.Apt.Pref.Star ->
          begin match pin with
          |Debian.Apt.Pref.Version _ -> assert false
          |Debian.Apt.Pref.Origin origin -> begin
              Printf.eprintf "Warning : origin is not currectly supported\n" ;
              let c = { dummycriteria with origin = Some origin } in
              {preferences with generic = (c, priority) :: preferences.generic}
          end
          |Debian.Apt.Pref.Release criteria -> 
              let c = List.fold_left map_criteria dummycriteria criteria in
              {preferences with generic = (c, priority) :: preferences.generic}
          end
      |Debian.Apt.Pref.Package name ->
          begin match pin with
          |Debian.Apt.Pref.Version version -> 
              let s = { dummyspec with name = name ; version = Some version } in
              {preferences with specific = (s, priority) :: preferences.specific }
          |Debian.Apt.Pref.Origin origin -> begin
              Printf.eprintf "Warning : origin is not currectly supported\n" ;
              let c = { dummycriteria with origin = Some origin } in
              let s = { dummyspec with name = name ; criteria = c} in
              {preferences with specific = (s, priority) :: preferences.specific}
          end
          |Debian.Apt.Pref.Release criteria ->
              let c = List.fold_left map_criteria dummycriteria criteria in
              let s = { dummyspec with name = name ; criteria = c } in
              {preferences with specific = (s, priority) :: preferences.specific}
          end
  ;;

  let parse ?tr s =
    let ch = IO.input_string s in
    let l = Debian.Apt.parse_preferences_in (fun x -> x) ch in
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
          let constr = {dummycriteria with archive = Some(info.Debian.Release.suite)} in
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
          if package.Cudf.installed then 100 else 500
      |Some tr, Some info ->
          if package.Cudf.installed then 100 else
          if not (tr = info.Debian.Release.suite) then 500
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
  Util.print_info "parse xml";
  let xdata = XmlParser.parse_ch (Input.open_file !input_file) in
  let content_to_string node = Xml.fold (fun a x -> a^(Xml.to_string x)) "" node in
  let is_include n =
    try match Xml.LazyList.to_list (Xml.children n) with
      |inc::_ when (Xml.tag inc) = "include" -> true
      |_ -> false
    with Xml.Not_element(_) -> false
  in
  let dudfproblem dudfprob node =
    Xml.fold (fun dudf node ->
      Util.print_info "  %s" (Xml.tag node);
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
            Util.print_info "   %s" (Xml.tag n);
            let filename = Xml.attrib n "filename" in
            match Xml.tag n with
            |"package-list" when is_include n ->
                begin match Xml.LazyList.to_list (Xml.children n) with
                |inc::_ ->
                  let href = Xml.attrib inc "href" in
                  let e = (filename, pkgget href) in
                  (e :: universe, release)
                |[] -> assert false end
            |"package-release" when is_include n ->
                begin match Xml.LazyList.to_list (Xml.children n) with
                |inc::_ ->
                  let href = Xml.attrib inc "href" in
                  let e = (filename, pkgget ~bz:false href) in
                  (universe, e :: release)
                |[] -> assert false end
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
                let r = Debian.Release.parse_release_in ch in
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
            (Debian.Release.default_release,c)
          ) (List.find_all (fun (fn,_) -> not(List.mem fn !fl)) ul) ;
        in
        {dudf with packageUniverse = universe @ without_release}
      |"action" -> {dudf with action = content_to_string node}
      |"desiderata" -> 
          {dudf with desiderata = {
            aptpref =
              Xml.fold (fun s n ->
                match Xml.tag n with
                |"apt_preferences" -> content_to_string n
                |_ -> assert false
              ) "" node
            }
          }
      |"outcome" ->
          {dudf with outcome =
            Xml.fold (fun t n ->
              match Xml.tag n with
              |"result" -> { t with result = content_to_string n }
              |"error" -> { t with error = content_to_string n }
              |_ -> assert false
            ) { result = "" ; error = "" } node
          }
      |s -> (Printf.eprintf "Warning : Unknown element %s\n" s ; dudf)
    ) dudfprob node
  in
  let dudfcomment dudfcomm node =
    Xml.fold (fun dudf node ->
      Util.print_info " %s" (Xml.tag node);
      match Xml.tag node with
      |"user" -> { dudf with user = content_to_string node }
      |"tags" -> 
          { dudf with tags =
            Xml.fold (fun acc n ->
              match Xml.tag n with
              |"tag" -> (content_to_string n)::acc
              |s -> (Printf.eprintf "Warning : Unknown element %s\n" s ; acc)
            ) [] node
          }
      |"hostid" -> { dudf with user = content_to_string node }
      |s -> (Printf.eprintf "Warning : Unknown element %s\n" s ; dudf)
    ) dudfcomm node
  in
  let dudfdoc dudfdoc node =
    Xml.fold (fun dudf node -> 
      Util.print_info " %s" (Xml.tag node);
      match Xml.tag node with
      |"distribution" -> dudf
      |"timestamp" -> {dudf with timestamp = content_to_string node}
      |"uid" -> {dudf with uid = content_to_string node}
      |"installer" -> {dudf with installer = content_to_string node}
      |"meta-installer" -> {dudf with metaInstaller = content_to_string node}
      |"problem" -> {dudf with problem = dudfproblem dudf.problem node }
      |"comment" -> {dudf with comment = dudfcomment dudf.comment node }
      |s -> (Printf.eprintf "Warning : Unknown elemenet %s\n" s ; dudf)
    ) dudfdoc node 
  in

  let id x = x in
  Util.print_info "convert to dom ... ";
  let dudf = dudfdoc dummydudf xdata in
  let preferences = AptPref.parse dudf.problem.desiderata.aptpref in

  let infoH = Hashtbl.create 1031 in
  let extras_property = [
    ("Size", ("size", `Nat (Some 0)));
    ("Installed-Size", ("installedsize", `Nat (Some 0)));
    ("Maintainer", ("maintainer", `String None))]
  in
  let extras = List.map fst extras_property in

  Util.print_info "parse all packages";
  let all_packages =
    List.fold_left (fun acc (univinfo,contents) ->
      let ch = IO.input_string contents in
      let l = Deb.parse_packages_in ~extras:extras id ch in
      let _ = IO.close_in ch in
      List.fold_left (fun s pkg -> 
        Hashtbl.add infoH (pkg.Deb.name,pkg.Deb.version) univinfo ;
        Deb.Set.add pkg s
      ) acc l
    ) Deb.Set.empty dudf.problem.packageUniverse
  in

  Util.print_info "installed packages";
  let installed_packages =
    let ch = IO.input_string dudf.problem.packageStatus in
    let l = Deb.parse_packages_in ~extras:extras id ch in
    let _ = IO.close_in ch in
    List.fold_left (fun s pkg -> Deb.Set.add pkg s) Deb.Set.empty l
  in

  Util.print_info "union";
  let l = Deb.Set.elements (Deb.Set.union all_packages installed_packages) in
  let tables = Debian.Debcudf.init_tables l in

  let installed =
    let h = Hashtbl.create 1031 in
    Deb.Set.iter (fun pkg ->
      Hashtbl.add h (pkg.Deb.name,pkg.Deb.version) ()
    ) installed_packages
    ;
    h
  in
  
  let add_extra (k,v) pkg =
    { pkg with Cudf.pkg_extra = (k,v) :: pkg.Cudf.pkg_extra } in

  Util.print_info "convert";
  let pl =
    List.map (fun pkg ->
      let inst = Hashtbl.mem installed (pkg.Deb.name,pkg.Deb.version) in
      let info = try Some(Hashtbl.find infoH (pkg.Deb.name,pkg.Deb.version)) with Not_found -> None in
      let cudfpkg = Debcudf.tocudf tables ~extras:extras_property ~inst:inst pkg in
      let priority = AptPref.assign_priority preferences info cudfpkg in
      let cudfpkg = add_extra ("priority", `Int priority) cudfpkg in
      cudfpkg
    ) l
  in

  let universe = Cudf.load_universe pl in

  Util.print_info "request";
  let request =
    let mapver = function
      |`Pkg p -> (p,None)
      |`PkgVer (p,v) -> begin
          try (p,Some(`Eq,Debcudf.get_cudf_version tables (p,v)))
          with Not_found -> failwith (Printf.sprintf "There is no version %s of package %s" p v)
      end
      |`PkgDst (p,d) ->
          try
            let l = Cudf.lookup_packages universe p in
            let pkg = List.find (fun pkg ->
                let number = Cudf.lookup_package_property pkg "number" in
                let info = Hashtbl.find infoH (pkg.Cudf.package,number) in
                info.Debian.Release.suite = d
              ) l
            in
            let number = Cudf.lookup_package_property pkg "number" in
            (pkg.Cudf.package,Some(`Eq,Debcudf.get_cudf_version tables (pkg.Cudf.package,number)))
          with Not_found ->
            failwith (Printf.sprintf "There is no package %s in release %s " p d)
    in
    let request_id =
      if !Options.problemid <> "" then !Options.problemid
      else if dudf.uid <> "" then dudf.uid
      else (string_of_int (Random.bits ()))
    in
    match Debian.Apt.parse_request_apt dudf.problem.action with
    |Debian.Apt.Upgrade (Some (suite))
    |Debian.Apt.DistUpgrade (Some (suite)) -> 
        let il = Deb.Set.fold (fun pkg acc -> `PkgDst (pkg.Deb.name,suite) :: acc) installed_packages [] in
        let l = List.map mapver il in
        { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; }
    |Debian.Apt.Install l ->
        let l = List.map mapver l in
        { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; } 
    |Debian.Apt.Remove l -> 
        let l = List.map (fun (`Pkg p) -> (p,None) ) l in
        { Cudf.request_id = request_id ; install = [] ; remove = l ; upgrade = [] ; req_extra = [] ;}
    |Debian.Apt.Upgrade None -> 
        { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = [] ; req_extra = [] ; }
    |Debian.Apt.DistUpgrade None -> 
        { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = [] ; req_extra = [] ; }
  in

  Util.print_info "dump";
  let oc =
    if !Options.outdir <> "" then begin
      let dirname = !Options.outdir in
      let file =
        let s = Filename.basename !input_file in
        try Filename.chop_extension s with Invalid_argument _ -> s
      in
      if not(Sys.file_exists dirname) then Unix.mkdir dirname 0o777 ;
      open_out (Filename.concat dirname (file^".cudf"))
    end else stdout
  in
  let preamble =
    let p = ("priority",(`Int (Some 500))) in
    let l = List.map snd extras_property in
    CudfAdd.add_properties Debcudf.preamble (p::l)
  in
  Cudf_printer.pp_cudf (Format.formatter_of_out_channel oc) (preamble, universe, request)
(*  Printf.fprintf oc "%s\n"
    (Cudf_printer.string_of_cudf (preamble, universe, request))
*)
;;

main ();;

