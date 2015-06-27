(**************************************************************************************)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.univ-paris-diderot.fr>     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** this functions follow the semantic versioning specification http://semver.org/ *)

module Pcre = Re_pcre

let re_semver = Pcre.regexp "(\\d+)\\.(\\d+)(?:\\.)?(\\d*)(\\.|-|\\+)?([0-9A-Za-z-.]*)?"

let parse s =
  let substrings = Pcre.extract re_semver s in
  let major = substrings.(1) in
  let minor = substrings.(2) in
  let patch = substrings.(3) in
  (* let separator = substrings.(4) in *)
  let special = substrings.(5) in
  (major,minor,patch,special)

let compare x y =
  let res x = if x = 0 then 0 else if x < 0 then -1 else 1 in
  if x = y then 0
  else
    let (major1,minor1,patch1,special1) = parse x in
    let (major2,minor2,patch2,special2) = parse y in
    let c1 = compare major1 major2 in
    if c1 <> 0 then res c1
    else
      let c2 = compare minor1 minor2 in
      if c2 <> 0 then res c2
      else
        let c3 = compare patch1 patch2 in
        if c3 <> 0 then res c3
        else
          res (compare special1 special2)

let equal x y =
  if x = y then true else (compare x y) = 0
