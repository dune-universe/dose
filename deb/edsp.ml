(**************************************************************************************)
(*  Copyright (C) 2011 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2011 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Representation of a apt-get <-> solvers protocol edsp > 0.4 *)

module Pcre = Re_pcre
open ExtLib
open Common

include Util.Logging(struct let label = __FILE__ end) ;;

type request = {
  request : string;
  install : Packages_types.vpkg list;
  remove : Packages_types.vpkg list;
  architecture : Packages_types.architecture option;
  architectures : Packages_types.architectures;
  autoremove : bool;
  upgrade : bool;
  distupgrade : bool;
  strict_pin : bool;
  preferences: string;
  cmdline: string;
}

let default_request = {
  request = "";
  install = [];
  remove = [];
  architecture = None;
  architectures = [];
  autoremove = false;
  upgrade = false;
  distupgrade = false;
  strict_pin = false;
  preferences = "";
  cmdline = ""
}

(* convert a apt command line request to edsp request *)
let from_apt_request arch request = function
  |Apt.Install vpkgreqlist ->
      List.fold_left (fun acc -> function
        |(Some Packages_types.I, ((n,None),c), _) -> {acc with install = ((n,arch),c) :: acc.install}
        |(Some Packages_types.R, ((n,None),c), _) -> {acc with remove =  ((n,arch),c) :: acc.remove}
        |(None, ((n,None),c), _) -> {acc with install = ((n,arch),c) :: acc.install}

        |(Some Packages_types.I, vpkg, _) -> {acc with install = vpkg :: acc.install}
        |(Some Packages_types.R, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
        |(None, vpkg, _) -> {acc with install = vpkg :: acc.install}
      ) request vpkgreqlist
  |Apt.Remove vpkgreqlist ->
      List.fold_left (fun acc -> function
        |(Some Packages_types.I, ((n,None),c), _) -> {acc with install = ((n,arch),c) :: acc.install}
        |(Some Packages_types.R, ((n,None),c), _) -> {acc with remove = ((n,arch),c) :: acc.remove}
        |(None, ((n,None),c), _) -> {acc with remove = ((n,arch),c) :: acc.remove}

        |(Some Packages_types.I, vpkg, _) -> {acc with install = vpkg :: acc.install}
        |(Some Packages_types.R, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
        |(None, vpkg, _) -> {acc with remove = vpkg :: acc.remove}
      ) request vpkgreqlist
  |Apt.Upgrade _ -> {request with upgrade = true }
  |Apt.DistUpgrade _ -> {request with distupgrade = true}
;;

let parse_s = Packages.parse_s
let parse_string (_,s) = s
let parse_string_opt = function (_,"") -> None | (_,s) -> Some s
let parse_string_list (_,s) = String.nsplit s " "
let parse_int_s (_,s) = string_of_int(int_of_string s)
let parse_req (loc,s) = 
  let aux = Packages.lexbuf_wrapper Packages_parser.vpkg_top in
  let l = Pcre.split ~rex:Apt.blank_regexp s in 
  List.map (fun s -> aux (loc,s)) l

let parse_edsp_version (_,s) =
  match String.nsplit s " " with
  |["EDSP";s] when (float_of_string s) >= 0.4 -> s
  |_ -> raise Not_found

(*
let get_architectures native_opt foreign =
  let cmd = "apt-config dump" in
  let arch = ref "" in
  let archs = ref [] in
  let aux () =
    let out = Std.input_list (Unix.open_process_in cmd) in
    List.iter (fun s ->
      let key, value =  ExtString.String.split s " " in
      if key = "APT::Architecture" then
        arch := ExtString.String.slice ~first: 1 ~last:(-2) value
      else if key = "APT::Architectures::" || key = "APT::Architectures" then
        let s = ExtString.String.slice ~first:1 ~last:(-2) value in
        if s <> "" then
          archs := (ExtString.String.slice ~first:1 ~last:(-2) value)::!archs
    ) out;
    debug "Automatically set native as %s and foreign archs as %s" !arch (String.concat "," !archs);
  in
  match native_opt, foreign with 
  |None,None     -> aux () ; (!arch,List.filter ((<>) !arch) !archs)
  |None,Some l   -> fatal "Native arch is missing while Foregin archs are specified"
  |Some a,None   -> (a,[])
  |Some a,Some l -> (a,l)
;;
*)

let parse_request_stanza par =
  (* request must be parse before any other fields *)
  let request = parse_s ~err:"(Invalid EDSP version)" parse_edsp_version "Request" par in
  {
    request = request; 
    install = parse_s ~opt:[] parse_req "Install" par;
    remove = parse_s ~opt:[] parse_req "Remove" par;
    upgrade = parse_s ~opt:false Packages.parse_bool "Upgrade" par;
    architecture = parse_s ~opt:None parse_string_opt "Architecture" par;
    architectures = parse_s ~opt:[] parse_string_list "Architectures" par;
    distupgrade = parse_s ~opt:false Packages.parse_bool "Dist-Upgrade" par;
    autoremove = parse_s ~opt:false Packages.parse_bool "Autoremove" par;
    strict_pin = parse_s ~opt:true Packages.parse_bool "Strict-Pinning" par;
    preferences = parse_s ~opt:"" Packages.parse_string "Preferences" par;
    cmdline = parse_s ~opt:"" Packages.parse_string "Command-Line" par;
  }
;;

(* parse and return a string -> for extra fields *)
let parse_bool_s = function
  |(_,("Yes"|"yes"|"true" |"True")) -> "true"
  |(_,("No" |"no" |"false"|"False")) -> "false" (* this one usually is not there *)
  |(_,s) -> raise (Format822.Type_error ("wrong value : "^ s))

let parse_installed = parse_s parse_bool_s "Installed"
let parse_hold = parse_s parse_bool_s "Hold"
let parse_apt_id = parse_s ~err:"(MISSING APT-ID)" parse_string "APT-ID"
let parse_apt_pin = parse_s ~err:"(MISSING APT-Pin)" parse_int_s "APT-Pin"
let parse_automatic = parse_s parse_bool_s "APT-Automatic"
let parse_candidate = parse_s parse_bool_s "APT-Candidate"
let parse_section = parse_s parse_string "Section"

(* (field,opt,err,multi,parsing function) *)
let extras = [
  ("Installed", Some parse_installed);
  ("Hold", Some parse_hold);
  ("APT-ID", Some parse_apt_id);
  ("APT-Pin", Some parse_apt_pin);
  ("APT-Candidate", Some parse_candidate);
  ("APT-Automatic", Some parse_automatic);
  ("Section", Some parse_section);
  ("APT-Release", None);
  ]

(* parse the entire file while filtering out unwanted stanzas *)
let rec packages_parser ?(request=false) (req,acc) p =
  (* if strict pin = true we filter packages that are installed and
     apt-candidate *)
  let filter par = 
    let match_field f p =
      try 
        begin match Packages.assoc f p with
        |(_,("Yes"|"yes"|"True" |"true")) -> true
        |(_,("No" |"no" |"False"|"false")) -> false
        |_ -> false
        end
      with Not_found -> false
    in
    let inst () = match_field "Installed" par in 
    let candidate () = match_field "APT-Candidate" par in
    ((inst ()) || (candidate ()))
  in
  let archs =
    if Option.is_none req.architecture then []
    else (Option.get req.architecture)::req.architectures 
  in
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> (req,acc) (* end of file *)
  |Some stanza when request = true -> 
      let req = parse_request_stanza stanza in
      packages_parser (req,acc) p
  |Some stanza when req.strict_pin = true -> begin
    match (Packages.parse_package_stanza (Some(filter)) archs extras stanza) with
    |None -> packages_parser (req,acc) p
    |Some st -> packages_parser (req,st::acc) p
  end
  |Some stanza when req.strict_pin = false -> begin
    match (Packages.parse_package_stanza None archs extras stanza) with
    |None -> assert false (* this is not possible in this branch *)
    |Some st -> packages_parser (req,st::acc) p
  end
  |_ -> assert false
;;

let input_raw_ch ic =
  Format822.parse_from_ch (
    packages_parser ~request:true (default_request,[])
  ) ic
;;

let input_raw file =
  let ch =
    match file with
    |"-" -> IO.input_channel stdin
    |_   -> Input.open_file file
  in
  let l = input_raw_ch ch in
  let _ = Input.close_ch ch in
  l
;;

let extras_tocudf =
  [
  ("Hold", ("hold", `Bool (Some false)));
  ("APT-Pin", ("apt-pin", `Int None));
  ("APT-ID", ("apt-id", `String None));
  ("APT-Candidate", ("apt-candidate", `Bool (Some false)));
  ("APT-Automatic", ("apt-automatic", `Bool (Some false)));
  ("Section", ("section", `String (Some ""))); 
  ]
;;

let is_installed pkg =
  try
    let _loc = Format822.dummy_loc in
    let v = Packages.assoc "Installed" pkg.Packages.extras in
    Packages.parse_bool (_loc,v)
  with Not_found -> false

let is_on_hold pkg =
  try
    let _loc = Format822.dummy_loc in
    let v = Packages.assoc "Hold" pkg.Packages.extras in
    (Packages.parse_bool (_loc,v))
  with Not_found -> false

let tocudf tables ?(options=Debcudf.default_options) ?(inst=false) pkg =
  let options = { options with Debcudf.extras_opt = extras_tocudf } in
  let pkg = 
    if is_installed pkg then
      let s = 
        if is_on_hold pkg then "hold ok installed" 
        else "install ok installed"
      in
      { pkg with Packages.extras = ("Status",s)::pkg.Packages.extras }
    else pkg
  in
  Debcudf.tocudf tables ~options pkg
;;

(* Only one version of a package can be installed at a given time.
   Hence, when a remove request is issued without version constraint,
   we return (candidate.Cudf.package,None) that designates the only
   package installed.
 *)
let requesttocudf tables universe request =
  let to_cudf (p,v) = (p,Debcudf.get_cudf_version tables (p,v)) in
  let get_candidate (name,constr) =
    try
      List.find
        (fun pkg ->
          try (Cudf.lookup_package_property pkg "apt-candidate") = "true"
          with Not_found -> false)
        (CudfAdd.who_provides universe (name,constr))
    with Not_found ->
      fatal "Package %s does not have a suitable candidate" name
  in
  let select_packages ?(remove=false) l =
    List.map (fun ((n,a),c) ->
      let (name,constr) = Debutil.debvpkg to_cudf ((n,a),c) in
      if remove then (name,None)
      else begin
        match constr, request.strict_pin with
        |None, false -> (name, None)
        |_, _ -> (name,Some(`Eq,(get_candidate (name,constr)).Cudf.version))
        (* FIXME: when apt will accept version constraints different from `Eq,
           we will need to pass them through. *)
      end
    ) l
  in
  if request.upgrade || request.distupgrade then
    let to_upgrade = function
      |[] ->
        let filter pkg = pkg.Cudf.installed in
        let l = Cudf.get_packages ~filter universe in
        List.map (fun pkg -> (pkg.Cudf.package,None)) l
      |l -> select_packages l
    in
    {Cudf.default_request with
    Cudf.request_id = request.request;
    Cudf.upgrade = to_upgrade request.install;
    Cudf.remove = select_packages ~remove:true request.remove;
    }
  else
    {Cudf.default_request with
    Cudf.request_id = request.request;
    Cudf.install = select_packages request.install;
    Cudf.remove = select_packages ~remove:true request.remove;
    }
;;

