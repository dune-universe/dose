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

(* cannibalized from deb-rpm-check - Jerome *)
(* cannibalized from cudflib - Zack *)

open ExtLib
open Common

type t =
  { mutable next : unit -> string;
    mutable cur : string;
    mutable eof : bool }

let eof i = i.eof

let cur i =
  assert (not i.eof);
  i.cur

let parse_error i =
  failwith (Printf.sprintf "Parse error : %s\n" i.cur)

let next i =
  assert (not i.eof);
  try
    i.cur <- i.next ()
  with End_of_file ->
    i.eof <- true

let expect s v = assert (not (eof s) && cur s = v); next s

let is_blank i = not (eof i) && cur i = ""

let skip_blank_lines i =
  while is_blank i do next i done

let field_re = Str.regexp "^\\([^:]*\\)*:[ \t]*\\(.*\\)$"

let remove_ws s =
  let l = String.length s in
  let p = ref (l - 1) in
  while !p >= 0 && (s.[!p] = ' ' || s.[!p] = '\t') do decr p done;
  if !p + 1 = l then s else
  String.sub s 0 (!p + 1)

(* return None with EOF *)
let parse_paragraph i =
  skip_blank_lines i;
  if eof i then None else begin
    let fields = ref [] in
    while
      let l = cur i in
      if not (Str.string_match field_re l 0) then
        parse_error i;
      let name = Str.matched_group 1 l in
      let data1 = remove_ws (Str.matched_group 2 l) in
      let data = ref [data1] in
      next i;
      while
        not (eof i || is_blank i) &&
        let l = cur i in l.[0] = ' ' || l.[0] = '\t'
      do
        data := remove_ws (cur i) :: !data;
        next i
      done;
      fields := (String.lowercase name, List.rev !data) :: !fields;
      not (eof i || is_blank i)
    do () done;
    assert (!fields <> []) ;
    Some (List.rev !fields)
  end

let single_line f = function
  |[s] -> s
  | _ as l ->
      failwith (
        Printf.sprintf "field '%s' should be a single line\n%s"
        f (String.concat " " l)
      )

(* May need to accept package name containing "_" *)
let token_re =
  Str.regexp
    ("[ \t]+\\|\\(" ^
     String.concat "\\|"
       [","; "|"; "("; ")"; "<<"; "<="; "="; ">="; ">>"; "<"; ">";
        "[A-Za-z0-9.:_+~-]+"] ^
     "\\)")

let rec next_token s p =
  if !p = String.length s then raise End_of_file else
  if Str.string_match token_re s !p then begin
    p := Str.match_end ();
    try
      Str.matched_group 1 s
    with Not_found ->
      next_token s p
  end else
    failwith (Format.sprintf "Bad token in '%s' at %d" s !p)

let start_from_fun f =
  let res = { next = f; cur = ""; eof = false } in
  next res; res

let start_token_stream s =
  let p = ref 0 in start_from_fun (fun () -> next_token s p)

let start_from_channel ch =
  start_from_fun (fun () ->
    try IO.read_line ch 
    with IO.No_more_input -> raise End_of_file
  )

(*****************************************************)

let strict_version_re_1 =
  Str.regexp
  ("^\\(\\([0-9]+\\):\\)?" ^
   "\\([0-9][A-Za-z0-9.:+~-]*\\)" ^
   "-\\([A-Za-z0-9.+~]+\\)$")
let strict_version_re_2 =
  Str.regexp
  ("^\\(\\([0-9]+\\):\\)?" ^
   "\\([0-9][A-Za-z0-9.:+~]*\\)\\( \\)?$")
(* Some upstream version do not start with a digit *)
let version_re_1 =
  Str.regexp
  "^\\(\\([0-9]+\\):\\)?\\([A-Za-z0-9._:+~-]+\\)-\\([A-Za-z0-9.+~]+\\)$"
(* XXX add '-' to the version spec ... *)
let version_re_2 =
  Str.regexp
  "^\\(\\([0-9]+\\):\\)?\\([A-Za-z0-9._:+~-]+\\)\\( \\)?$"

let check_version s =
  if not (Str.string_match strict_version_re_1 s 0 || Str.string_match strict_version_re_2 s 0) then begin
    (Printf.eprintf "Warning : bad version '%s'\n" s);
    if not (Str.string_match version_re_1 s 0 || Str.string_match version_re_2 s 0) then 
      failwith (Format.sprintf "Bad version '%s'@." s)
  end

let parse_version s = 
  check_version s;
  s
;;

(*****************************************************)

let strict_package_re = Str.regexp "^[a-z0-9][a-z0-9.+-]+$"
let package_re = Str.regexp "^[A-Za-z0-9][A-Za-z0-9._+-]+$"

let check_package_name s =
  if not (Str.string_match strict_package_re s 0) then begin
    (Printf.eprintf "Warning : bad package name '%s'\n" s);
    if not (Str.string_match package_re s 0) then
      failwith (Format.sprintf "Bad package name '%s'@." s)
  end

let parse_package s =
  check_package_name s;
  s
;;

(*****************************************************)

let parse_constr_aux vers s =
  let name = cur s in
  check_package_name name;
  next s;
  if not (eof s) && cur s = "(" then begin
    if not vers then
      failwith (Format.sprintf "Package version not allowed in '%s'" name);
    next s;
    let comp = (cur s) in
    next s;
    let version = cur s in
    check_version version;
    next s;
    expect s ")";
    (name, Some (comp, version))
  end else
    (name, None)

let parse_constr s =
  let s = start_token_stream s in
  parse_constr_aux true s

(*****************************************************)

let space_re = Str.regexp " "
let and_sep_re = Str.regexp "[ \t]*,[ \t]*"
let or_sep_re = Str.regexp "[ \t]*|[ \t]*"

let parse_source s =
  match Str.split space_re s with
  |[n] -> (n,None)
  |[n;s'] ->
      let re = Str.regexp "(\\([^\\)]*\\))" in
      if Str.string_match re s' 0 then
        (n,Some (Str.matched_group 0 s'))
      else begin
        Printf.eprintf "Warning : bad source name '%s'\n" s;
        (n,None)
      end
  |_ -> failwith (Format.sprintf "Malformed source field : '%s'" s)

let list_parser ?(sep = space_re) p s = List.map p (Str.split sep s)

let parse_vpkglist parse_vpkg = list_parser ~sep:and_sep_re parse_vpkg

let parse_vpkgformula parse_vpkg s =
  let and_args = Str.split and_sep_re s in
  List.map (fun and_arg ->
    let or_args = Str.split or_sep_re and_arg in
    List.map parse_vpkg or_args
  ) and_args

let parse_veqpkglist parse_veqpkg = list_parser ~sep:and_sep_re parse_veqpkg

exception Eof
(** parse a 822 compliant file.
    @return list of Ipr packages.
    @param parse : paragraph parser
    @param f : filter to be applied to each paragraph 
    @param ExtLib.IO.input channel *)
let parse_822_iter parse f ch =
  let progressbar = Util.progress "Debian.Parse.parse_822_iter" in
  let total = 25000 in (* estimate *)
  let i = ref 0 in
  let l = ref [] in
  try
    while true do
      progressbar (incr i ; !i , total) ;
      match parse_paragraph ch with
      |None -> raise Eof
      |Some par ->
        begin match parse par with
        |None -> ()
        |Some e -> l := (f e) :: !l
        end
    done ;
    !l
  with Eof -> !l

module RawInput ( Set : Set.S ) = struct
  let input_raw f files =
    let timer = Util.Timer.create "Debian.Format822.input_raw" in
    Util.Timer.start timer;
    let s =
      List.fold_left (fun acc file ->
        let ch = (Input.open_file file) in
        let l = f (fun x -> x) ch in
        let _ = Input.close_ch ch in
        List.fold_left (fun s x -> Set.add x s) acc l
      ) Set.empty files
    in
    Util.Timer.stop timer (Set.elements s)
end
