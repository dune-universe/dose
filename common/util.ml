(*****************************************************************************)
(*  Copyright (C) 2009  <pietro.abate@pps.jussieu.fr>                        *)
(*  Part of  the following code is borrowed from Cduce.                      *)
(*  Copyright: Alain Frish                                                   *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(** return a unique identifier *)
let uuid () =
  let rand =
    let s = Random.State.make_self_init () in
    fun () -> Random.State.bits s
  in
  Digest.to_hex (Digest.string (string_of_int (rand ())))

type label = string

let msg_verbose = ref false
let time_verbose = ref false
let make_verbose ?(msg=true) ?(time=false) () =
  time_verbose := time ; msg_verbose := msg
let make_quite () = 
  time_verbose := false ; msg_verbose := false

module type Messages = sig
  type t
  val create: ?enabled:bool -> label -> t
  val eprintf: t -> ('a, unit, string, unit) format4 -> 'a
  val enable : label -> unit
  val disable : label -> unit
  val all_disabled : unit -> unit
  val all_enabled : unit -> unit
  val avalaible : unit -> label list
end

(** Debug messages are printed immediately on stderr. 
 * They can enabled or disabled (default) *)
module MakeMessages(X : sig val label : string end) = struct
  type t = {
    label : string;
    mutable enabled : bool
  } 
  let messages = Hashtbl.create 10
  let allenabled = ref false

  let create ?(enabled=false) label =
    if not (Hashtbl.mem messages label) then
      { label = label ; enabled = enabled }
    else begin
      Format.eprintf "The label (%s) %s already exists@." X.label label;
      exit 1
    end

  let eprintf t fmt =
    Printf.kprintf (
      if (t.enabled || !allenabled) && !msg_verbose then begin
        (fun s -> Format.eprintf "%s : %s@." t.label s)
      end else ignore
    ) fmt

  let onoff s b =
    try let t = Hashtbl.find messages s in t.enabled <- b
    with Not_found ->
      Printf.eprintf "Warning: debug label %s not found\n" s

  let all_enabled () = allenabled := true
  let all_disabled () = allenabled := false
  let enable s = onoff s true
  let disable s = onoff s false

  let avalaible () = Hashtbl.fold (fun k _ acc -> k::acc) messages []
  let all_enabled () = allenabled := true
  let all_disabled () = allenabled := false
end

(* this way we can have the same label for different messages *)
module Debug = MakeMessages(struct let label = "Debug" end)
module Info = MakeMessages(struct let label = "Info" end)
module Warning = MakeMessages(struct let label = "Warning" end)

let make_info label =
  let t = Info.create ~enabled:true label in
  fun fmt -> Info.eprintf t fmt

let make_warning label =
  let t = Warning.create ~enabled:true label in
  fun fmt -> Warning.eprintf t fmt

let make_debug label =
  let t = Debug.create label in
  fun fmt -> Debug.eprintf t fmt

(** Printf bars are printed immediately on stderr.
 * they can be enabled or disabled (default) *)
module Progress = struct
  type t = {
    name : string ;
    buffer : Buffer.t ;
    mutable total : int ;
    mutable perc : int ;
    mutable rotation : int ;
    mutable enabled : bool ;
  }

  let columns = 75 
  let full = " %100.0\n" 
  let rotate = "|/-\\"
  let bars = Hashtbl.create 10

  let create ?(enabled=false) ?(total=0) s =
    let c = {
      name = s;
      buffer = Buffer.create columns ;
      total = total ;
      perc = 0 ;
      rotation = 0 ;
      enabled = enabled }
    in
    Hashtbl.add bars s c;
    c

  let enable s =
    try let t = Hashtbl.find bars s in t.enabled <- true
    with Not_found ->
      Printf.eprintf "Warning: Progress Bar %s not found\n" s

  let avalaible () = Hashtbl.fold (fun k _ acc -> k::acc) bars []

  let set_total c total = c.total <- total
  let reset c =
    Buffer.clear c.buffer;
    c.perc <- 0;
    c.rotation <- 0

  let progress ?(i=1) c =
    if c.enabled && !msg_verbose then begin
      c.perc <- c.perc + i;
      Buffer.clear c.buffer;
      Buffer.add_char c.buffer '\r';
      Buffer.add_string c.buffer c.name;
      let f = floor (1000.0 *. (float c.perc) /. (float c.total)) in
      let f = f /. 10.0 in
      if f = 100.0 then Buffer.add_string c.buffer full
      else begin
        c.rotation <- (1 + c.rotation) land 3;
        Printf.bprintf c.buffer "%c %%%4.1f" rotate.[c.rotation] f
      end ;
      Format.eprintf "%s" (Buffer.contents c.buffer)
    end

end

let loggers = ref []
let register f = loggers := f :: !loggers
let dump fmt = List.iter (fun pp -> pp fmt) !loggers

(** Timers are printed all together when the function dump is called.
 * they can be enabled or disabled (default) *)
module Timer = struct
  type t = {
    name: string;
    mutable count : int;
    mutable total : float;
    mutable last  : float;
    mutable is_in : bool;
    mutable enabled : bool;
  }

  let gettimeofday = ref (fun _ -> 0.)
  let () = gettimeofday := Unix.gettimeofday

  let pp fmt c =
    if c.enabled && !time_verbose then
      Format.fprintf fmt "Timer %s. Total time: %f. Count: %i@."
        c.name c.total c.count

  let create ?(enabled=true) s =
    let c = { 
      name = s;
      count = 0;
      total = 0.;
      last = 0.;
      is_in = false ;
      enabled = enabled
    } 
    in
    register (fun fmt -> pp fmt c);
    c

  let start c =
    assert(not c.is_in);
    c.is_in <- true;
    c.last <- !gettimeofday();
    c.count <- c.count + 1

  let stop c x =
    assert(c.is_in);
    c.is_in <- false;
    c.total <- c.total +. (!gettimeofday() -. c.last);
    x
end

(** Counters are printed all together when the function dump is called.
 * they can be enabled or disabled (default) *)
module Counter = struct
  type t = {
    name: string;
    mutable count : int;
    mutable enabled : bool;
  }

  let pp fmt c =
    if c.enabled && !time_verbose then
      Format.fprintf fmt "Counter %s: %i@." c.name c.count

  let create ?(enabled=false) s =
    let c = { name = s; count = 0; enabled = enabled } in
    register (fun fmt -> pp fmt c);
    c

  let incr c =
    c.count <- c.count + 1

  let add c n =
    c.count <- c.count + n
end

(* we always print user / sys timers if the user ask to print Summary info *)
let pp_process_time fmt () =
  let pt = Unix.times () in
  Format.fprintf fmt "Process time (user):  %5.2f@." pt.Unix.tms_utime;
  Format.fprintf fmt "Process time (sys):   %5.2f@." pt.Unix.tms_stime
;;

register (fun fmt -> if !time_verbose then pp_process_time fmt ());;
