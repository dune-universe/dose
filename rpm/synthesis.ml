
open ExtLib
open Common
open IprLib

(*
let parse_op s =
  let re_star = Str.regexp "\*" in
  if Str.string_match re_star s 0 then None
  else match Str.split (Str.regex "\s") s with
  |["==";v] -> Some("=",v)
  |[op;v] -> Some(op,v)
*)

let parse_op = function
  |"*" -> None
  |sel ->
      try Scanf.sscanf sel "%s %s" (fun op v ->
        match op with
        |"==" -> Some("=",v)
        |_ -> Some(op,v))
      with End_of_file -> (print_endline sel ; assert false)

let parse_vpkg vpkg =
  try Scanf.sscanf vpkg "%[^[][%[^]]]" (fun n sel -> (n,parse_op sel))
  with End_of_file -> (vpkg,None)

let parse_deps l = List.map parse_vpkg l

let parse_info pkg = function
  |[nvra;epoch;size;group] ->
      let ra = String.rindex nvra '.' in
      let vr = String.rindex_from nvra (ra-1) '-' in
      let nv = String.rindex_from nvra (vr-1) '-' in
      let name = String.sub nvra 0 nv in
      let version = String.sub nvra (nv+1) (vr-nv-1) in
      let release = String.sub nvra (vr+1) (ra-vr-1) in
      (* let arch = String.sub nvra (ra+1) (String.length nvra-ra-1) in *)
      let version = 
        if epoch <> "0" then Printf.sprintf "%s:%s-%s" epoch version release
        else Printf.sprintf "%s-%s" version release
      in
      { pkg with Ipr.name = name ; version = version }
  |_ -> assert false

exception Eof

let rec parse_paragraph pkg ch =
  let parse_deps_ll l = List.map (fun x -> [x]) (parse_deps l) in
  let line =
    try IO.read_line ch 
    with IO.No_more_input -> raise Eof | End_of_file -> assert false
  in
  try 
    match Str.split (Str.regexp "@") line with
    |"provides"::l -> parse_paragraph {pkg with Ipr.provides = parse_deps l} ch
    |"requires"::l -> parse_paragraph {pkg with Ipr.depends = parse_deps_ll l} ch
    |"obsoletes"::l -> parse_paragraph pkg ch
    |"conflicts"::l -> parse_paragraph {pkg with Ipr.conflicts = parse_deps l} ch
    |"summary"::l -> parse_paragraph pkg ch
    |"filesize"::l -> parse_paragraph pkg ch
    |"suggests"::l -> parse_paragraph pkg ch
    |"info"::l -> parse_info pkg l
    |s::l -> ((Printf.eprintf "Unknown field %s\n%!" s) ; parse_paragraph pkg ch)
    |_ -> assert false
  with End_of_file -> assert false

let rec parse_packages_rec acc ch =
  try
    let par = parse_paragraph Ipr.default_package ch in
    parse_packages_rec (par::acc) ch
  with Eof -> acc

let parse_packages f filename =
  let ch = Input.open_chan filename in
  let l = parse_packages_rec [] ch in
  Input.close_chan ch;
  l
;;

let input_raw files =
  let timer = Util.Timer.create "Rpm.Synthesis.input_raw" in
  Util.Timer.start timer;
  let s =
    List.fold_left (fun acc f ->
      let l = parse_packages (fun x -> x) f in
      List.fold_left (fun s x -> Ipr.Set.add x s) acc l
    ) Ipr.Set.empty files
  in
  Util.Timer.stop timer (Ipr.Set.elements s)
