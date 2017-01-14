(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2010,2011 Ralf Treinen <ralf.treinen@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

module Pcre = Re_pcre
open ExtLib
open Common

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

(* first column of /usr/share/dpkg/cputable *)
(* the line numbers correspond to the line numbers in /usr/share/dpkg/cputable
 * to be quickly able to find changes *)
let cpulist = ref [
(* lines 17..23 *)  "i386"; "ia64"; "alpha"; "amd64"; "armeb"; "arm"; "arm64";
(* lines 24..29 *)  "avr32"; "hppa"; "m32r"; "m68k"; "mips"; "mipsel";
(* lines 30..34 *)  "mips64"; "mips64el"; "or1k"; "powerpc"; "powerpcel";
(* lines 35..41 *)  "ppc64"; "ppc64el"; "s390"; "s390x"; "sh3"; "sh3eb"; "sh4";
(* lines 42..44 *)  "sh4eb"; "sparc"; "sparc64" ]

(* from /usr/share/dpkg/tupletable
 *
 * the line numbers correspond to the line numbers in
 * /usr/share/dpkg/tupletable to be quickly able to find changes
 *
 *   debian tuple (abi,libc,os,cpu)      debian arch *)
let tupletable = ref [
  (("eabi","uclibc","linux","arm"),       "uclibc-linux-armel"); (* line 6  *)
  (("base","uclibc","linux","<cpu>"),     "uclibc-linux-<cpu>");
  (("eabihf","musl","linux","arm"),       "musl-linux-armhf");
  (("base","musl","linux","<cpu>"),       "musl-linux-<cpu>");
  (("eabihf","gnu","linux","arm"),        "armhf");              (* line 10 *)
  (("eabi","gnu","linux","arm"),          "armel");
  (("abin32","gnu","linux","mips64el"),   "mipsn32el");
  (("abin32","gnu","linux","mips64"),     "mipsn32");
  (("abi64","gnu","linux","mips64el"),    "mips64el");
  (("abi64","gnu","linux","mips64"),      "mips64");             (* line 15 *)
  (("spe","gnu","linux","powerpc"),       "powerpcspe");
  (("x32","gnu","linux","amd64"),         "x32");
  (("hardened1","gnu","linux","<cpu>"),   "hardened1-linux-<cpu>");
  (("base","gnu","linux","<cpu>"),        "<cpu>");
  (("base","gnu","kfreebsd","<cpu>"),     "kfreebsd-<cpu>");
  (("base","gnu","knetbsd","<cpu>"),      "knetbsd-<cpu>");      (* line 20 *)
  (("base","gnu","kopensolaris","<cpu>"), "kopensolaris-<cpu>");
  (("base","gnu","hurd","<cpu>"),         "hurd-<cpu>");
  (("base","bsd","dragonflybsd","<cpu>"), "dragonflybsd-<cpu>");
  (("base","bsd","freebsd","<cpu>"),      "freebsd-<cpu>");
  (("base","bsd","openbsd","<cpu>"),      "openbsd-<cpu>");      (* line 25 *)
  (("base","bsd","netbsd","<cpu>"),       "netbsd-<cpu>");
  (("base","bsd","darwin","<cpu>"),       "darwin-<cpu>");
  (("base","sysv","solaris","<cpu>"),     "solaris-<cpu>");
  (("eabi","uclibc","uclinux","arm"),     "uclinux-armel");
  (("base","uclibc","uclinux","<cpu>"),   "uclinux-<cpu>");      (* line 30 *)
  (("base","tos","mint","m68k"),          "mint-m68k");
  (("base","gnu","linux","<cpu>"),        "linux-<cpu>") (* this entry is not from /usr/share/dpkg/tupletable *)
  (* the "linux-" prefix is commented in scripts/Dpkg/Arch.pm with "XXX: Might disappear in the future, not sure yet." *)
]

let debarch_to_debtuple = Hashtbl.create ((List.length !tupletable)*(List.length !cpulist))
let tupletable_done = ref false

let mangle_cpu_placeholder ((abi,libc,os,cpu),debarch) =
  if cpu = "<cpu>" then begin
    List.iter (fun c ->
        let dt = (abi,libc,os,c) in
        let _,da = String.replace ~str:debarch ~sub:"<cpu>" ~by:c in
        Hashtbl.replace debarch_to_debtuple da dt
      ) !cpulist
  end else begin
    Hashtbl.replace debarch_to_debtuple debarch (abi,libc,os,cpu)
  end
;;

let read_tupletable ?(ttfile=None) ?(ctfile=None) () =
  if !tupletable_done && ttfile = None && ctfile = None then () else begin
    (* if cputable file was given, overwrite built-in table *)
    begin match ctfile with
      | Some fn -> begin
          cpulist := [];
          let ic = open_in fn in
          if input_line ic <> "# Version=1.0" then
            fatal "Require cputable version 1.0";
          (* to stay most compatible with dpkg, it would be best to use its
           * regex from from scripts/Dpkg/Arch.pm to parse this file.
           * Unfortunately Re.pcre doesnt support look-ahead/look-behind
           * assertions *)
          let aux line =
            if line.[0] = '#' || not (String.contains line '\t') then ()
            else begin
              let spacei = String.index line '\t' in
              let cpu = String.sub line 0 spacei in
              if not (List.mem cpu !cpulist) then cpulist := cpu::!cpulist;
            end;
          in
          List.iter aux (Std.input_list ic);
          close_in ic;
        end
      | None -> () end;
    (* if tupletable was given, overwrite built-in table, otherwise parse
     * built-in table *)
    begin match ttfile with
      | Some fn -> begin
          (* this is an implicit assumption of dpkg *)
          mangle_cpu_placeholder (("base","gnu","linux","<cpu>"), "linux-<cpu>");
          let ic = open_in fn in
          if input_line ic <> "# Version=1.0" then
            fatal "Require tupletable version 1.0";
          (* to stay most compatible with dpkg, it would be best to use its
           * regex from from scripts/Dpkg/Arch.pm to parse this file.
           * Unfortunately Re.pcre doesnt support look-ahead/look-behind
           * assertions *)
          let aux line =
            if line.[0] = '#' || not (String.contains line '\t') then ()
            else begin
              let spaceli = String.index line '\t' in
              let spaceri = String.rindex line '\t' in
              let debtuple = String.sub line 0 spaceli in
              let debarch = String.sub line (spaceri+1) ((String.length line)-spaceri-1) in
              match String.nsplit debtuple "-" with
              | [abi;libc;os;cpu] -> mangle_cpu_placeholder ((abi,libc,os,cpu),debarch)
              | _ -> fatal "Cannot parse debtuple: %s" debtuple
            end
          in
          List.iter aux (Std.input_list ic);
          close_in ic;
        end
      | None -> begin
          List.iter mangle_cpu_placeholder !tupletable;
        end
    end;
    tupletable_done := true;
  end
;;

(* this function performs what debarch_is form libdpkg-perl does *)
let src_matches_arch alias real =
  read_tupletable ();
  if alias=real || alias="any" || alias="all" then true else begin
    let real = Hashtbl.find_option debarch_to_debtuple real in
    (* see libdpkg-perl function debwildcard_to_debtuple *)
    let alias = match String.nsplit alias "-" with
      | ["any";libc;os;cpu] ->  Some ("any",libc,os,cpu)
      | [abi;"any";os;cpu] ->   Some (abi,"any",os,cpu)
      | [abi;libc;"any";cpu] -> Some (abi,libc,"any",cpu)
      | [abi;libc;os;"any"] ->  Some (abi,libc,os,"any")
      | ["any";os;cpu] ->   Some ("any","any",os,cpu)
      | [libc;"any";cpu] -> Some ("any",libc,"any",cpu)
      | [libc;os;"any"] ->  Some ("any",libc,os,"any")
      | ["any";cpu] ->     Some ("any","any","any",cpu)
      | [os;"any"] ->      Some ("any","any",os,"any")
      | ["any"] ->         Some ("any","any","any","any")
      | _ -> begin
          (* only look up in the table if none of the parts is "any" *)
          Hashtbl.find_option debarch_to_debtuple alias
        end
    in
    match real,alias with
    | Some (r1,r2,r3,r4), Some (a1,a2,a3,a4) ->
      ((a1=r1 || a1="any") && (a2=r2 || a2="any") && (a3=r3 || a3="any") && (a4=r4 || a4="any"))
    | _ -> false
  end
;;
