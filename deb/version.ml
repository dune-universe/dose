(**************************************************************************)
(*  Copyright (C) 2009-2011  Jaap Boender <jaap.boender@pps.jussieu.fr>   *)
(*                           and Ralf Treinen <treinen@pps.jussieu.fr>    *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

let is_digit = function
  | '0'..'9' -> true
  | _ -> false
;;

(* (skip_while_from i f w) yields the index of the leftmost character in the string s, starting
 * from i, that does not satisfy the predicate f, or (length w) if no such index exists.
 *)
let skip_while_from i f w =
  let m = String.length w in
  let rec loop i =
    if i = m then i
    else if f w.[i] then loop (i + 1) else i
  in loop i
;;
  
(* splits a version into (epoch,rest), without the separating ':'. The epoch is delimited
 * by the leftmost occurrence of ':' in x, and is "" in case there is no ':' in x.
 *)
let extract_epoch x =
  try
    let ci = String.index x ':' in
    let epoch = String.sub x 0 ci
    and rest = String.sub x (ci + 1) (String.length x - ci - 1)
    in (epoch,rest)
  with
  | Not_found -> ("",x)
;;

(* splits a version into (prefix,revision). The revision starts on the right-most
 * occurrence of '-', or is empty in case the version does not contain '-'.
 *) 
let extract_revision x =
  try
    let di = String.rindex x '-' in
    let before = String.sub x 0 di in
    let after = String.sub x (di+1) (String.length x - di -1) in
    (before,after)
  with
  | Not_found -> (x,"")
;;

(* character comparison uses a modified character ordering: '~' first, then letters, then anything else *)
let compare_chars c1 c2 = match c1 with
  | '~' -> (match c2 with
      | '~' -> 0
      | _ -> -1)
  | 'a'..'z'|'A'..'Z' -> (match c2 with
      | '~' -> 1
      | 'a'..'z'|'A'..'Z' -> Char.compare c1 c2
      | _ -> -1)
  | _ -> (match c2 with
      | '~'|'a'..'z'|'A'..'Z' -> 1
      | _ -> Char.compare c1 c2)
;;

(* return the first index of x, starting from xi, of a nun-null character in x.
 * or (length x) in case x contains only 0's starting from xi on.
 *)
let skip_zeros x xi = skip_while_from xi (fun c -> c = '0') x;;

(* compare versions chunks, that is parts of version strings that are epoch, upstream version,
 * or revisision. Alternates string comparison and numerical comaprison.
 *)
let compare_chunks x y =
  (* x and y may be empty *)
  let xl = String.length x
  and yl = String.length y
  in
  let rec loop_lexical xi yi =
    assert (xi <= xl && yi <= yl);
    match (xi=xl,yi=yl) with (* which of x and y is exhausted? *)
      | true,true -> 0 
      | true,false -> 
	  (* if y continues numerically than we have to continue by comparing numerically. In this case
	   * the x part is interpreted as 0 (since empty). If the y part consists only of 0's
	   * then both parts are equal, otherwise the y part is larger. If y continues non-numerically
	   * then y is larger anyway, so we only have to skip 0's in the y part and check whether this
	   * exhausts the y part.
	   *)
	let ys = skip_zeros y yi in if ys = yl then 0 else if y.[ys]='~' then 1 else -1
      | false,true -> (* symmetric to the preceding case *)
	let xs = skip_zeros x xi in if xs = xl then 0 else if x.[xs]='~' then -1 else 1
      | false,false -> (* which of x and y continues numerically? *)
	match (is_digit x.[xi], is_digit y.[yi]) with 
	  | true,true -> (* both continue numerically. Skip leading zeros in the remaining parts, 
			  * and then continue by comparing numerically. *)
	    compare_numerical (skip_zeros x xi) (skip_zeros y yi)
	  | true,false -> (* '~' is smaller than any numeric part *)
	    if y.[yi]='~' then 1 else -1
	  | false,true -> (* '~' is smaller than any numeric part *)
	    if x.[xi]='~' then -1 else 1
	  | false,false -> (* continue comparing lexically *)
	    let comp = compare_chars x.[xi] y.[yi]
	    in if comp = 0 then loop_lexical (xi+1) (yi+1) else comp
  and compare_numerical xi yi =
    assert (xi = xl || (xi < xl && x.[xi] <> '0')); (* leading zeros have been striped *) 
    assert (yi = yl || (yi < yl && y.[yi] <> '0')); (* leading zeros have been striped *) 
    let xn = skip_while_from xi is_digit x (* length of numerical part *)
    and yn = skip_while_from yi is_digit y (* length of numerical part *)
    in 
    let comp = compare (xn-xi) (yn-yi)
    in if comp = 0
      then (* both numerical parts have same length: compare digit by digit *)
	loop_numerical xi yi yn
      else (* if one numerical part is longer than the other we have found the answer
	    * since leading 0 have been striped when switching to numerical comparison.
	    *)
	comp
  and loop_numerical xi yi yn =
    assert (xi <= xl && yi <= yn && yn <= yl);
    (* invariant: the two numerical parts that remain to compare are of the same length *)
    if yi=yn
    then (* both numerical parts are exhausted, we switch to lexical comparison *)
      loop_lexical xi yi
    else (* both numerical parts are not exhausted, we continue comparing digit by digit *)
      let comp = Char.compare x.[xi] y.[yi] 
      in if comp = 0 then loop_numerical (xi+1) (yi+1) yn else comp
  in loop_lexical 0 0
;;

let compare (x : string) (y : string) =
  if x = y then 0
  else
    let (e1,rest1) = extract_epoch x 
    and (e2,rest2) = extract_epoch y in
    let e_comp = compare_chunks e1 e2 in 
    if e_comp <> 0 then e_comp
    else
      let (u1,r1) = extract_revision rest1
      and (u2,r2) = extract_revision rest2 in
      let u_comp = compare_chunks u1 u2 in
      if u_comp <> 0 then u_comp
      else compare_chunks r1 r2
;;

let equal (x : string) (y : string) =
  if x = y then true else (compare x y) = 0
;;


(*********************************************************************************************)
(************** splitting and recomposing version strings ************************************)

(* remark concerning transition [RT]: from here on the code is untouched  *)
 
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

let extract_string c x =
  try
    let di = String.rindex x c in
    if di < String.length x - 1 then
      let before = String.sub x 0 di in
      let after = String.sub x (di + 1) (String.length x - di - 1) in
      (before,after)
    else
      (x,"")
  with
  | Not_found -> (x,"")
;;

let extract_revision s = extract_string '-' s
let extract_binnmu s = extract_string '+' s

let split s =
  let (epoch,rest) = extract_epoch s in
  let (rest,binnmu) = extract_binnmu rest in
  let (upstream,revision) = extract_revision rest in
  (epoch,upstream,revision,binnmu)
;;

let concat = function
  |("",u,"","") -> Printf.sprintf "%s" u (* 1.1 *)
  |("",u,r,"") -> Printf.sprintf "%s-%s" u r (* 1.1-1 *)
  |("",u,"",b) -> Printf.sprintf "%s+%s" u b (* 1.1+b1 *)
  |("",u,r,b) -> Printf.sprintf "%s-%s+%s" u r b (* 1.1-1+b1 *)
  |(e,u,"","") -> Printf.sprintf "%s:%s" e u (* 1:1.1 *)
  |(e,u,"",b) -> Printf.sprintf "%s:%s+%s" e u b (* 1:1.1+b1 *)
  |(e,u,r,"") -> Printf.sprintf "%s:%s-%s" e u r (* 1:1.1-1 *)
  |(e,u,r,b) -> Printf.sprintf "%s:%s-%s+%s" e u r b (* 1:1.1-1+b1 *)
;;

let normalize s = 
  let (e,u,r,b) = split s in
  concat ("",u,r,"")
;; 

