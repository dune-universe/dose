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
  install : Format822.vpkg list;
  remove : Format822.vpkg list;
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

let parse_request_stanza par =
  let aux par =
    Some {
        request = Packages.parse_s ~err:"(Malformed REQUEST)" Packages.parse_string "request" par;
        install = Packages.parse_s ~opt:[] Packages.parse_conj "install" par;
        remove = Packages.parse_s ~opt:[] Packages.parse_conj "remove" par;
        upgrade = Packages.parse_s ~opt:false Packages.parse_bool "upgrade" par;
        distupgrade = Packages.parse_s ~opt:false Packages.parse_bool "dist-upgrade" par;
        autoremove = Packages.parse_s ~opt:false Packages.parse_bool "autoremove" par;
        strict_pin = Packages.parse_s ~opt:true Packages.parse_bool "strict-pinning" par;
        preferences = Packages.parse_s ~opt:"" Packages.parse_string "preferences" par;
    }
  in Packages.parse_packages_fields aux par
;;

let parse_installed par = Packages.parse_s Packages.parse_bool_s "Installed" par ;;
let parse_apt_id par = Packages.parse_s ~err:"(MISSING APT-ID)" Packages.parse_string "APT-ID" par ;;
let parse_apt_pin par = Packages.parse_s ~err:"(MISSING APT-Pin)" Packages.parse_int_s "APT-Pin" par ;;
let parse_automatic par = Packages.parse_s Packages.parse_bool_s "APT-Automatic" par ;;
let parse_candidate par = Packages.parse_s Packages.parse_bool_s "APT-Candidate" par ;;
let parse_section par = Packages.parse_s Packages.parse_string "Section" par ;;

let input_raw_ch ch =
  (* (field,opt,err,multi,parsing function) *)
  let extras = [
    ("Installed", parse_installed);
    ("APT-ID", parse_apt_id);
    ("APT-Pin", parse_apt_pin);
    ("APT-Candidate", parse_candidate);
    ("APT-Automatic", parse_automatic);
    ("Section", parse_section);
    ]
  in
  let request = 
    match Format822.parse_paragraph (Format822.start_from_channel ch) with 
    |None -> fatal "empty request"
    |Some par -> 
        match parse_request_stanza par with
        |None -> fatal "empty request"
        |Some r -> r
  in
  let pkglist = Packages.parse_packages_in ~extras ch in
  (request,pkglist)
;;

let extras_tocudf =
  [("installed", ("installed", `Bool (Some false)));
  ("APT-Pin", ("apt_pin", `Posint (Some 0)));
  ("APT-ID", ("apt_id", `String None));
  ("APT-Candidate", ("apt_candidate", `Bool (Some false)));
  ("APT-Automatic", ("apt_automatic", `Bool (Some false)));
  ("Section", ("section", `String (Some ""))); 
  ]
;;
