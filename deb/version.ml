(**************************************************************************)
(*  Copyright (C) 2009  Jaap Boender (jaap.boender@pps.jussieu.fr)        *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

(* cannibalized from ocamldeb *)

let is_digit = function
  | '0'..'9' -> true
  | _ -> false
;;

let first_matching_char_from i f w =
  let m = String.length w in
  let rec loop i =
    if i = m then
      raise Not_found
    else
      if f w.[i] then
        i
      else
        loop (i + 1)
  in
  loop i
;;

let first_matching_char = first_matching_char_from 0;;

let longest_matching_prefix f w =
  try
    let i = first_matching_char (fun c -> not (f c)) w in
    String.sub w 0 i, String.sub w i (String.length w - i)
  with
  | Not_found -> (w,"")
;;

let extract_epoch x =
  try
    let ci = String.index x ':' in
    if ci < String.length x - 1 then
      let epoch = String.sub x 0 ci
      and rest = String.sub x (ci + 1) (String.length x - ci - 1)
      in
      (epoch,rest)
    else
      ("",x)
  with
  | Not_found -> ("",x)
;;

let extract_revision x =
  try
    let di = String.rindex x '-' in
    if di < String.length x - 1 then
      let upstream = String.sub x 0 di
      and revision = String.sub x (di + 1) (String.length x - di - 1)
      in
      (upstream,revision)
    else
      (x,"")
  with
  | Not_found -> (x,"")
;;

let extract_chunks x =
  let (epoch,rest) = extract_epoch x in
  let (upstream,revision) = extract_revision rest in
  (epoch,upstream,revision)
;;

let ( ** ) x y = if x = 0 then y else x;;
let ( *** ) x y = if x = 0 then y () else x;;
let ( ~~~ ) f x = not (f x)

let order = function
  | `C '~' -> (0,'~')
  | `C('0'..'9' as c) -> (1,c)
  | `E -> (2,'\000')
  | `C('a'..'z'|'A'..'Z' as c) -> (3,c)
  | `C(c) -> (4,c)
;;

let compare_couples (x1,x2) (y1,y2) = (compare x1 y1) ** (compare x2 y2);;

let compare_special x y =
  let m = String.length x
  and n = String.length y
  in
  let rec loop i =
    let cx = if i >= m then `E else `C(x.[i])
    and cy = if i >= n then `E else `C(y.[i])
    in
    (compare_couples (order cx) (order cy)) ***
    (fun () ->
      if i > m or i > n then
        0
      else
        loop (i + 1))
  in
  loop 0
;;

(* -1 : x < y *)
(** According to APT's behaviour, 5.002 and 5.2 are equivalent version numbers.  This means that
  * the Debian ordering is not a proper order on strings but a preorder.  This means that it is
  * not possible to use version string-indexed hashtables, as we may get duplicate entries.
  *)

let compare_numeric_decimal x y =
  let m = String.length x
  and n = String.length y
  in
  let rec loop1 i j =
    if i < m && x.[i] = '0' then
      loop1 (i + 1) j
    else
      if j < n && y.[j] = '0' then
        loop1 i (j + 1)
      else
        if i = m && j = n then
          0
        else
          loop2 i j
  and loop2 i j =
    if i = m then
      if j = n then
        0
      else
        (* x is finished, but y is not *)
        -1
    else
      if j = n then
        (* x is not finished, but y is *)
        1
      else
        if m - i < n - j then -1
        else if m - i > n - j then 1
        else
          if x.[i] < y.[j] then -1
          else if x.[i] > y.[j] then 1
          else
              loop2 (i + 1) (j + 1)
  in
  loop1 0 0
;;
(* ***)

let rec compare_chunks x y =
  if x = y then 0
  else
    let x1,x2 = longest_matching_prefix (~~~ is_digit) x
    and y1,y2 = longest_matching_prefix (~~~ is_digit) y
    in
    let c = compare_special x1 y1 in
    if c <> 0 then
      c
    else
      let (x21,x22) = longest_matching_prefix is_digit x2
      and (y21,y22) = longest_matching_prefix is_digit y2
      in
      let c = compare_numeric_decimal x21 y21 in
      if c <> 0 then
        c
      else
        compare_chunks x22 y22
;;

let compare x1 x2 =
  let (e1,u1,r1) = extract_chunks x1
  and (e2,u2,r2) = extract_chunks x2
  in
  (compare_numeric_decimal e1 e2) ***
    (fun () -> (compare_chunks u1 u2) ***
      (fun () -> compare_chunks r1 r2))
;;

let equal (x : string) (y : string) =
  if x = y then true else (compare x y) = 0
