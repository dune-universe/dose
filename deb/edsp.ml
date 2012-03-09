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
  install : Format822.vpkgreq list;
  remove : Format822.vpkgreq list;
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

let parse_s = Packages.parse_s
let parse_string (_,s) = s
let parse_int_s (_,s) = string_of_int(int_of_string s)
let parse_req s = Packages.lexbuf_wrapper Packages_parser.request_top s

let parse_request_stanza par =
  {
    request = parse_s ~err:"(Empty REQUEST)" parse_string "Request" par;
    install = parse_s ~opt:[] parse_req "Install" par;
    remove = parse_s ~opt:[] parse_req "Remove" par;
    upgrade = parse_s ~opt:false Packages.parse_bool "Upgrade" par;
    distupgrade = parse_s ~opt:false Packages.parse_bool "Dist-Upgrade" par;
    autoremove = parse_s ~opt:false Packages.parse_bool "Autoremove" par;
    strict_pin = parse_s ~opt:true Packages.parse_bool "Strict-Pinning" par;
    preferences = parse_s ~opt:"" Packages.parse_string "Preferences" par;
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
  ]

(* parse the entire file while filtering out unwanted stanzas *)
let rec packages_parser ?(request=false) (req,acc) p =
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
    let inst () = match_field "installed" par in 
    let candidate () = match_field "apt-candidate" par in
    ((inst ()) || (candidate ()))
  in
  match Format822_parser.stanza_822 Format822_lexer.token_822 p.Format822.lexbuf with
  |None -> (req,acc) (* end of file *)
  |Some stanza when request = true -> 
      let req = parse_request_stanza stanza in
      packages_parser (req,acc) p
  |Some stanza when req.strict_pin = true -> begin
    match (Packages.parse_package_stanza (Some(filter)) [] extras stanza) with
    |None -> packages_parser (req,acc) p
    |Some st -> packages_parser (req,st::acc) p
  end
  |Some stanza when req.strict_pin = false -> begin
    match (Packages.parse_package_stanza None [] extras stanza) with
    |None -> assert false (* this is not possible in this branch *)
    |Some st -> packages_parser (req,st::acc) p
  end
  |_ -> assert false
;;

let input_raw_ch ic =
  Format822.parse_from_ch (packages_parser ~request:true (default_request,[])) ic
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
  let options = { Debcudf.default_options with Debcudf.extras = extras_tocudf } in
  let inst =
    try
      let _loc = Format822.dummy_loc in
      let v = Packages.assoc "installed" pkg.Packages.extras in
      Packages.parse_bool (_loc,v)
    with Not_found -> false
  in
  Debcudf.tocudf tables ~options ~inst pkg 
;;
