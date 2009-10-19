(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

let split s c =
  let len = String.length s in
  let rec iter pos =
    try
      if pos = len then [""] else
      let pos2 = String.index_from s pos c in
      if pos2 = pos then "" :: iter (pos+1) else
        (String.sub s pos (pos2-pos)) :: (iter (pos2+1))
    with _ -> [String.sub s pos (len-pos)]
  in
  iter 0
;;

let before s pos = String.sub s 0 pos
let after s pos =
    let len = String.length s in
    String.sub s pos (len - pos)
;;
let cut_at s c =
  try
    let pos = String.index s c in
    before s pos,
    after s (pos+1);
  with _ -> s, ""
;;

open Buffer

type url = {
    proto : string;
    server : string;
    port : int;
    full_file : string;
    file : string;
    user : string;
    passwd : string;
    args : (string*string) list;

    string : string;
  }

(* encode using x-www-form-urlencoded form *)
let encode s =
  let pos = ref 0 in
  let len = String.length s in
  let res = String.create (3*len) in
  let hexa_digit x =
    if x >= 10 then Char.chr (Char.code 'A' + x - 10)
    else Char.chr (Char.code '0' + x) in
  for i=0 to len-1 do
    match s.[i] with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '-' | '*' | '_' ->
        res.[!pos] <- s.[i]; incr pos
(*    | ' ' -> res.[!pos] <- '+'; incr pos *)
    | c ->
        res.[!pos] <- '%';
        res.[!pos+1] <- hexa_digit (Char.code c / 16);
        res.[!pos+2] <- hexa_digit (Char.code c mod 16);
        pos := !pos + 3
  done;
  String.sub res 0 !pos

(* decode using x-www-form-urlencoded form *)

let digit_hexa x =
  match x with
  | 'a' .. 'f' -> (Char.code x) + 10 - (Char.code 'a')
  | 'A' .. 'F' -> (Char.code x) + 10 - (Char.code 'A')
  | '0' .. '9' -> (Char.code x) - (Char.code '0')
  | _ -> failwith "Not an hexa number (encode.ml)"

let decode s =
  let len = String.length s in
  let r = Buffer.create len in
  let rec iter i =
    if i < len then
      match s.[i] with
      | '+' -> Buffer.add_char r  ' '; iter (i+1)
      | '%' ->
          let n =
            try
              let fst = digit_hexa s.[i+1] in
              let snd = digit_hexa s.[i+2] in
              Buffer.add_char r (char_of_int (fst*16 + snd));
              3
            with _ ->
                Buffer.add_char r '%';
                1
          in
          iter (i+n)

      | c -> Buffer.add_char r c; iter (i+1)
  in
  iter 0;
  Buffer.contents r


let to_string url =
  let res = Buffer.create 80 in
  add_string res url.proto;
  add_string res "://";
  if url.user <> "" || url.passwd <> "" then begin
      add_string res url.user;
      add_string res ":";
      add_string res url.passwd;
      add_string res "@";
    end;
  add_string res url.server;
    (match url.proto, url.port with
      "http", 80
    | "ftp", 21
    | "ssh", 22 -> ()
    | ("http" | "ftp" | "ssh"), _ ->
        (add_char res ':'; add_string res (string_of_int url.port));
    | _, port when port <> 0 ->
        (add_char res ':'; add_string res (string_of_int url.port));
    | _ -> ());
  add_string res url.full_file;
  contents res

let cut_args url_end =
  if url_end = "" then []
  else
  let args = split url_end '&' in
  List.map (fun s ->
        let (name, value) = cut_at s '=' in
      decode name, decode value
    ) args

let create proto user passwd server port full_file =
  let short_file, args = cut_at full_file '?' in
  let args = cut_args args in
  let url =
    {
      proto = proto;
      server = server;
      port = port;
      full_file = full_file;
      file = short_file;
      user = user;
      passwd = passwd;
      args = args;

      string = "";
    }
  in
  { url with string = to_string url }

let put_args s args =
  if args = [] then s else
  let res = Buffer.create 256 in
  Buffer.add_string res s;
  Buffer.add_char res '?';
  let rec manage_args = function
    | [] -> assert false
    | [a, ""] ->
        Buffer.add_string res (encode a)
    | [a, b] ->
        Buffer.add_string res (encode a); Buffer.add_char res '='; Buffer.add_string res
          (encode b)
    | (a,b)::l ->
        Buffer.add_string res (encode a); Buffer.add_char res '='; Buffer.add_string res
          (encode b);
        Buffer.add_char res '&'; manage_args l in
(*  lprintf "len args %d" (List.length args); lprint_newline ();*)
  manage_args args;
  Buffer.contents res

let of_string ?(args=[]) s =
  let s = put_args s args in
  let url =
    let get_two init_pos =
      let pos = ref init_pos in
      while s.[!pos] <> ':' && s.[!pos] <> '/' && s.[!pos] <> '@' do
        incr pos
      done;
      let first = String.sub s init_pos (!pos - init_pos) in
      if s.[!pos] = ':'
      then
        (let deb = !pos+1 in
          while s.[!pos] <> '@' && s.[!pos] <> '/' do
            incr pos
          done;
          (first, String.sub s deb (!pos-deb), !pos))
      else
        (first, "", !pos) in
    let cut init_pos default_port =
      let stra, strb, new_pos = get_two init_pos in
      let user, pass, host, port, end_pos =
        if s.[new_pos] = '@'
        then
          (let host, port_str, end_pos = get_two (new_pos+1) in
            let port =
              if port_str="" then default_port else int_of_string port_str in
            stra, strb, host, port, end_pos)
        else
          (let port = if strb="" then default_port else int_of_string strb in
            "", "", stra, port, new_pos) in
      let len = String.length s in
      let file = String.sub s end_pos (len - end_pos) in
      host, port, file, user, pass in
    try
      let colon = String.index s ':' in
      let len = String.length s  in
      if len > colon + 2 &&
        s.[colon+1] = '/' &&
        s.[colon+2] = '/' then
        let proto =  String.sub s 0 colon in
        let port = match proto with
            "http" -> 80
          | "ftp" -> 21
          | "ssh" -> 22
          | _ -> 0
        in
        let host, port, full_file, user, pass = cut (colon+3) port in
        create proto user pass host port full_file

      else
        raise Not_found
    with Not_found ->
        let short_file, args = cut_at s '?' in
        let args = cut_args args in
        {
          proto = "file";
          server = "";
          port = 0;
          full_file = s;
          file = short_file;
          user = "";
          passwd = "";
          args = args;

          string = s;
        }
  in
  url

let to_string url = url.string

let to_string_no_args url =
  let res = Buffer.create 80 in
  add_string res url.proto;
  add_string res "://";
  add_string res url.server;
  (match url.proto, url.port with
      "http", 80
    | "ftp", 21
    | "ssh", 22 -> ()
    | ("http" | "ftp" | "ssh"), _ ->
        (add_char res ':'; add_string res (string_of_int url.port));
    | _, port when port <> 0 ->
        (add_char res ':'; add_string res (string_of_int url.port));
    | _ -> ());
  add_string res url.file;
  contents res

