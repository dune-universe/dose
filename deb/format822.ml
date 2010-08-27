(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Parts taken from deb-rpm-check - Copyright (C) 2005 Jerome Vouillon               *)
(*  Parts taken from cudflib - Copyright (C) 2008 Stefano Zacchiroli                  *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

(** Low level debian format parser *)

open ExtLib
open Common

let debug fmt = Util.make_debug "Debian.Format822" fmt
let info fmt = Util.make_info "Debian.Format822" fmt

type name = string
type version = string
type vpkg = (string * (string * string) option)
type veqpkg = (string * (string * string) option)

type t =
  { mutable next : unit -> (string * int);
    mutable line : int;
    mutable cur : string;
    mutable eof : bool }

let dummy_t = { next = (fun _ -> ("",0)); cur = ""; eof = false; line = -1 } 

let eof i = i.eof

let cur i =
  assert (not i.eof);
  i.cur

let parse_error ?s i =
  begin match s with
    |None when i.line > 0 ->
        Printf.eprintf "Parse error at line %d: \"%s\"\n" i.line i.cur
    |None ->
        Printf.eprintf "Parse error : \"%s\"\n" i.cur
    |Some s when i.line > 0 ->
        Printf.eprintf "Error: %s at line %d: \"%s\"\n" s i.line i.cur
    |Some s ->
        Printf.eprintf "Error: %s : \"%s\"\n" s i.cur
  end ; exit 1
;;

let next i =
  assert (not i.eof);
  try
    let s,l = i.next () in
    i.cur <- s;
    i.line <- l
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
      let p =
        try
          String.index l ':'
        with Not_found ->
          parse_error i
      in
      let name = String.sub l 0 p in
      let n = String.length l in
      let p = ref (p + 1) in
      while !p < n && let c = l.[!p] in c = ' ' || c = '\t' do incr p done;
      let data1 = remove_ws (String.sub l !p (n - !p)) in
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
     [","; "|"; "("; ")"; "\\["; "\\]"; "!"; "<<"; "<="; "="; ">="; ">>"; "<"; ">";
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
    parse_error ~s:(Printf.sprintf "Bad token in '%s' at %d" s !p) dummy_t

let start_from_fun f =
  let res = { next = f; cur = ""; eof = false; line = 0 } in
  next res; res

let start_token_stream s =
  let p = ref 0 in start_from_fun (fun () -> (next_token s p,0))

let start_from_channel =
  let i = ref 0 in fun ch ->
    start_from_fun (fun () ->
      try incr i ; (IO.read_line ch,!i)
      with IO.No_more_input -> raise End_of_file
    )

exception ParseError of string * int
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

let check_version i s =
  if not (Str.string_match strict_version_re_1 s 0 || Str.string_match strict_version_re_2 s 0) then begin
    (debug "bad version '%s'" s);
    if not (Str.string_match version_re_1 s 0 || Str.string_match version_re_2 s 0) then 
      raise (ParseError ((Printf.sprintf "Bad version '%s'" s), i.line))
  end

let parse_version s = 
  begin try check_version dummy_t s
  with ParseError (s,i) -> parse_error ~s:s dummy_t end;
  s
;;

(*****************************************************)

let strict_package_re = Str.regexp "^[a-z0-9][a-z0-9.+-]+$"
let package_re = Str.regexp "^[A-Za-z0-9][A-Za-z0-9._+-]+$"

let check_package_name i s =
  if not (Str.string_match strict_package_re s 0) then begin
    (debug "bad package name '%s'" s);
    if not (Str.string_match package_re s 0) then
      raise (ParseError ((Printf.sprintf "Bad version '%s'" s), i.line))
  end

let parse_package s =
  begin try check_package_name dummy_t s
  with ParseError (s,i) -> parse_error ~s:s dummy_t end ;
  s
;;

(*****************************************************)

let parse_constr_aux ?(check=true) vers s =
  let name = cur s in
  if check then check_package_name s name;
  next s;
  if not (eof s) && cur s = "(" then begin
    try 
      if not vers then
        parse_error ~s:(Printf.sprintf "Package version not allowed in '%s'" name) s;
      next s;
      let comp = (cur s) in
      next s;
      let version = cur s in
      if check then check_version s version;
      next s;
      expect s ")";
      (name, Some (comp, version))
    with ParseError (s,_) -> begin
      (Printf.eprintf "WARNING !!! '%s'\n" s);
      (name, None)
    end
  end else begin
    (* XXX if the constraint if malformed I should print a warning !!! *)
    (name, None)
  end

let parse_constr s =
  let s = start_token_stream s in
  parse_constr_aux ~check:true true s

let parse_builddeps s =
  let s = start_token_stream s in
  let c = parse_constr_aux true s in
  if not (eof s) && (cur s) = "[" then begin
    let l = ref [] in
    next s;
    while not (eof s) && not((cur s) = "]") do
      if not (eof s) && cur s = "!" then
        ( next s; l := (false,cur s)::!l )
      else 
        ( l := (true,cur s)::!l )
      ;
      next s
    done;
    (c,!l)
  end
  else (c,[])
;;

(*****************************************************)

let space_re = Str.regexp "[ \t]+"
let and_sep_re = Str.regexp "[ \t]*,[ \t]*"
let or_sep_re = Str.regexp "[ \t]*|[ \t]*"

let parse_source s =
  match Str.split space_re s with
  |[n] -> (n,None)
  |[n;s'] ->
      let re = Str.regexp "(\\([^)]+\\))" in
      if Str.string_match re s' 0 then 
        (n,Some (Str.matched_group 1 s'))
      else begin
        debug "bad source name '%s'\n" s;
        (n,None)
      end
  |_ -> parse_error ~s:(Printf.sprintf "Malformed source field : '%s'" s) dummy_t

let list_parser ?(sep = space_re) p s = List.map p (Str.split sep s)

let parse_vpkglist parse_vpkg = list_parser ~sep:and_sep_re parse_vpkg

let parse_vpkgformula parse_vpkg s =
  let and_args = Str.split and_sep_re s in
  List.map (fun and_arg ->
    let or_args = Str.split or_sep_re and_arg in
    List.map parse_vpkg or_args
  ) and_args

let parse_veqpkglist parse_veqpkg = list_parser ~sep:and_sep_re parse_veqpkg

let progressbar = Util.Progress.create "Debian.Parse.parse_822_iter"
exception Eof

(** parse a 822 compliant file.
    @return list of packages.
    @param parse : paragraph parser
    @param f : filter to be applied to each paragraph 
    @param ExtLib.IO.input channel *)
let parse_822_iter parse f ch =
  let total = 25000 in (* estimate *)
  Util.Progress.set_total progressbar total;
  let l = ref [] in
  try
    while true do
      Util.Progress.progress progressbar;
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
    if List.length files > 1 then info "Merging input lists" ;
    let s =
      List.fold_left (fun acc file ->
        info "Parsing %s..." file;
        let ch = (Input.open_file file) in
        let l = f (fun x -> x) ch in
        let _ = Input.close_ch ch in
        List.fold_left (fun s x -> Set.add x s) acc l
      ) Set.empty files
    in
    info "total Packages %n" (Set.cardinal s);
    Util.Timer.stop timer (Set.elements s)

  let input_raw_ch f ch =
    let timer = Util.Timer.create "Debian.Format822.input_raw_ch" in
    Util.Timer.start timer;
    let s =
      info "Parsing...";
      let l = f (fun x -> x) ch in
      let _ = Input.close_ch ch in
      List.fold_left (fun s x -> Set.add x s) Set.empty l
    in
    info "total Packages %n" (Set.cardinal s);
    Util.Timer.stop timer (Set.elements s)
end
