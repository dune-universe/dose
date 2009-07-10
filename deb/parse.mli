(**************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

type release = {
  origin : string;
  label : string;
  suite : string;
  version : string;
  codename : string;
  date : string;
  architecture : string;
  component : string;
  description : string;
  md5sums : (string * string * string) list;
  sha1 : (string * string * string) list;
  sha256 : (string * string * string) list;
}

val default_release : release

(** parse a Release file from a channel *)
val parse_release_in : IO.input -> release

(** parse_packages_in [f] [ch] : parse a file Packages from a channel [ch] 
 * and return a apply the function [f] to each Ipr.package *)
val parse_packages_in : (Ipr.package -> 'a) -> IO.input -> 'a list

(** parse a list of Packages files *)
val input_raw : string list -> Ipr.package list

type apt_req_only = [ `Pkg of string ]
type apt_req_pkg =
  [`Pkg of string
  |`PkgDst of string * string
  |`PkgVer of string * string ]

type apt_req =
  |Install of apt_req_pkg list
  |Remove of apt_req_only list
  |Upgrade of string option
  |DistUpgrade of string option

(** parse 'apt-get' command line request *)
val parse_request_apt : string -> apt_req

module Pref :
  sig
    type pin_t =
        Release of (string * string) list
      | Origin of string
      | Version of string
    type package_t = Package of string | Star
    type pin_priority_t = int
    type apt_preferences = {
      package : package_t;
      pin : pin_t;
      pin_priority : pin_priority_t;
    }
  end

val parse_preferences_in : (Pref.apt_preferences -> 'a) -> IO.input -> 'a list

(** parse the debian popularity context file and return a tuple that contains
 * the ranking, name and installation numbers *)
val parse_popcon : string -> int * string * int
