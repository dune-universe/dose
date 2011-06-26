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

(** Representation of a apt-get <-> solvers protocol edsp 0.3 *)

open ExtLib
open Common

let debug fmt = Util.make_debug "Debian.Edsp" fmt
let info fmt = Util.make_info "Debian.Edsp" fmt
let warning fmt = Util.make_warning "Debian.Edsp" fmt
let fatal fmt = Util.make_fatal "Debian.Edsp" fmt

type request = {
  request : string;
  install : Format822.vpkglist;
  remove : Format822.vpkglist;
  autoremove : bool;
  upgrade : bool;
  distupgrade : bool;
  strict_pin : bool;
  preferences: string;
}

let default_request = {
  request = "";
  install = [];
  remove = [];
  autoremove = false;
  upgrade = false;
  distupgrade = false;
  strict_pin = false;
  preferences = ""
}

let parse_req s = 
  match String.nsplit s "=" with
  |[n] -> (n,None)
  |[n;v] -> (n,Some("=",v))
  |_ -> assert false
;;

let parse_s = Packages.parse_s
let parse_req s = Packages.lexbuf_wrapper Packages_parser.request_top s

let parse_request_stanza par =
  Some {
    request = parse_s ~err:"(Malformed REQUEST)" Packages.parse_string "Request" par;
    install = parse_s ~opt:[] parse_req "Install" par;
    remove = parse_s ~opt:[] parse_req "Remove" par;
    upgrade = parse_s ~opt:false Packages.parse_bool "Upgrade" par;
    distupgrade = parse_s ~opt:false Packages.parse_bool "Dist-Upgrade" par;
    autoremove = parse_s ~opt:false Packages.parse_bool "Autoremove" par;
    strict_pin = parse_s ~opt:true Packages.parse_bool "Strict-Pinning" par;
    preferences = parse_s ~opt:"" Packages.parse_string "Preferences" par;
  }
;;

let parse_string (_,s) = s

(* parse and return a string -> for extra fields *)
let parse_bool_s = function
  |(_,("Yes"|"yes"|"true" |"True")) -> "true"
  |(_,("No" |"no" |"false"|"False")) -> "false" (* this one usually is not there *)
  |(_,s) -> raise (Format822.Type_error ("wrong value : "^ s))

let parse_int_s (_,s) = string_of_int(int_of_string s)

let parse_installed = Packages.parse_s parse_bool_s "Installed"
let parse_hold = Packages.parse_s parse_bool_s "Hold"
let parse_apt_id = Packages.parse_s ~err:"(MISSING APT-ID)" parse_string "APT-ID"
let parse_apt_pin = Packages.parse_s ~err:"(MISSING APT-Pin)" parse_int_s "APT-Pin"
let parse_automatic = Packages.parse_s parse_bool_s "APT-Automatic"
let parse_candidate = Packages.parse_s parse_bool_s "APT-Candidate"
let parse_section = Packages.parse_s parse_string "Section"
;;

let input_raw_ch ch =
  (* (field,opt,err,multi,parsing function) *)
  let extras = [
    ("Installed", parse_installed);
    ("Hold", parse_hold);
    ("APT-ID", parse_apt_id);
    ("APT-Pin", parse_apt_pin);
    ("APT-Candidate", parse_candidate);
    ("APT-Automatic", parse_automatic);
    ("Section", parse_section);
    ]
  in
  let request = 
    match Packages.parse_from_ch parse_request_stanza ch with 
    |[r] -> r
    |_ -> fatal "empty request (empty document)"
  in
  (* XXX: not convinced that this is the correct level to put this filter *)
  let pkglist = 
    if request.strict_pin then
      let _loc = Format822.dummy_loc in
      let filter pkg = 
        let inst () =
          try
            let v = Packages.assoc "Installed" pkg.Packages.extras in
            Packages.parse_bool (_loc,v)
          with Not_found -> false
        in
        let candidate () =
          try
            let v = Packages.assoc "APT-Candidate" pkg.Packages.extras in
            Packages.parse_bool (_loc,v)
          with Not_found -> false
        in
        (inst ()) || (candidate ())
      in
      Packages.parse_packages_in ~filter ~extras ch 
    else
      Packages.parse_packages_in ~extras ch
  in
  (request,pkglist)
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

let tocudf tables pkg =
  let inst =
    try
      let _loc = Format822.dummy_loc in
      let v = Packages.assoc "Installed" pkg.Packages.extras in
      Packages.parse_bool (_loc,v)
    with Not_found -> false
  in
  Debcudf.tocudf tables ~inst ~extras:extras_tocudf pkg 
;;
