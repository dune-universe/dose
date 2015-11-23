(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open ExtLib
open Criteria_types

module Pcre = Re_pcre

#define __label __FILE__
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

let lexbuf_wrapper type_parser v =
  Format822.lexbuf_wrapper type_parser Criteria_lexer.token v
let parse_criteria v = lexbuf_wrapper Criteria_parser.criteria_top v

let string_of_set = function
  | Solution -> "solution"
  | Changed -> "changed"
  | New -> "new"
  | Removed -> "removed"
  | Up -> "up"
  | Down -> "down"

(* FIXME: put these at a better place *)
(* Cudf properties are identifiers as per cudf spec and must start with
 * a lowercase latin letter, followed by lowercase latin letters, dashes
 * or arabic numerals and must be of length one or greater.
 * We restrict ourselves to US ASCII characters because checking for
 * latin characters would be hard. We also ignore the length restriction
 * and the special restriction for the beginning of a property because
 * our new field below automatically has a sufficient length and starts
 * with a lowercase latin character because of the "x-" prefix *)
let invalidchars = Pcre.regexp "[^0-9a-z-]"
let cudfsanitize s =
  (* make the string all lowercase *)
  let s = String.lowercase s in
  (* replace all possibly illegal letters by a dash *)
  Pcre.substitute ~rex:invalidchars ~subst:(fun _ -> "-") s

let extcount_to_cudffieldname ?(sep="=") fieldname regex =
  (* Get the first eight hex digits of the md5sum of the fieldname, the match
   * type and the search string. *)
  let regexhash = String.sub (Digest.to_hex (Digest.string (fieldname^sep^regex))) 0 8 in
  (* Cudf field names are much more restrictive than deb822 field names which
   * is why the deb822 field name has to be sanitized.
   * The first eight hex chars of the md5sum of the fieldname, the match type
   * and the search string are appended because:
   *   - sanitizing deb822 field names might make them not unique anymore
   *   - regexp may contain mostly special characters that would otherwise all
   *     be deleted, creating a non-unique field name
   *   - regexp might be very long but cutting of the regex might make the
   *     result non-unique
   *   - a hash solves all these problems because it contains only valid
   *     characters while being unique for any input (minus unlikely collisions)
   * *)
  "x-" ^ (cudfsanitize fieldname) ^ "-" ^ regexhash

let to_string ?(solver="") crit =
  match solver with
  | "mccs-cbc" | "mccs-lpsolve" -> failwith "not implemented"
  | _ -> begin
      String.concat "," (List.map (fun pred ->
          let pred, crit = match pred with
            | Maximize crit -> "+", crit
            | Minimize crit -> "-", crit
          in
          pred ^ match crit with
          | Count (set, None) -> "count(" ^ (string_of_set set) ^ ")"
          | Count (set, Some (field, r)) ->
            let sep, r = match r with ExactMatch r -> ("=", r) | Regexp r -> ("~", r) in
            "sum(" ^ (string_of_set set) ^ "," ^ (extcount_to_cudffieldname ~sep field r) ^ ")"
          | Sum (set, attr) -> "sum(" ^ (string_of_set set) ^ "," ^ attr ^ ")"
          | Unsatrec set -> "unsatrec(" ^ (string_of_set set) ^ ")"
          | Aligned (set, attr1, attr2) -> "aligned(" ^ (string_of_set set) ^ "," ^ attr1 ^ "," ^ attr2 ^ ")"
          | NotUptodate set -> "notuptodate(" ^ (string_of_set set) ^ ")"
        ) crit)
    end

let extcount_iter f =
  List.iter (function
      | Minimize (Count(_,Some(fieldname,regex))) | Maximize (Count(_,Some(fieldname,regex))) ->
        (* compile the regex so that this doesn't need to be done later *)
        (* TODO: the regex should probably be multiline? *)
        let regexstring, sep, compiledre = match regex with
          | Regexp r -> (r, "~", Some(Pcre.regexp r))
          | ExactMatch r -> (r, "=", None)
        in
        let cudffieldname = extcount_to_cudffieldname ~sep fieldname regexstring in
        f cudffieldname fieldname regexstring compiledre
      | _ -> ()
    )
