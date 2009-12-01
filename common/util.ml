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

type verbosity = Quiet | Summary | Details
let verbosity = ref Quiet
let set_verbosity = (:=) verbosity

let print ppf v label fmt =
  Printf.kprintf (
    if v <= !verbosity then 
      (fun s -> Format.fprintf ppf "%s: %s\n%!" label s)
    else ignore
  ) fmt
;;

let print_warning ?(ppf=Format.err_formatter) fmt =
  print ppf Summary "Warning" fmt

let print_info ?(ppf=Format.err_formatter) fmt =
  print ppf Summary "Info" fmt

let gettimeofday = ref (fun _ -> 0.)
let () = gettimeofday := Unix.gettimeofday

let loggers = ref []
let register level f = loggers := (level,f) :: !loggers

let dump ppf =
  List.iter (function
    |(level,f) when level <= !verbosity -> f ppf
    |_ -> ()
  ) !loggers

module Progress = struct
  type label = string

  type t = {
    name : label ;
    buffer : Buffer.t ;
    mutable total : int ;
    mutable perc : int ;
    mutable rotation : int ;
    mutable enabled : bool ;
  }

  let columns = 75 
  let full = " %100.0\n" 
  let rotate = "|/-\\"
  let bars = ref []

  let create ?(enabled=false) ?(total=0) s =
    let c = {
      name = s;
      buffer = Buffer.create columns ;
      total = total ;
      perc = 0 ;
      rotation = 0 ;
      enabled = enabled }
    in
    bars := (s,c)::!bars ;
    c

  let enable s =
    try
      let (_,c) = List.find (fun (n,_) -> s = n) !bars in
      c.enabled <- true
    with Not_found -> Printf.eprintf "Warning: Progress bar %s not found\n" s

  let set_total c total = c.total <- total
  let reset c =
    Buffer.clear c.buffer;
    c.perc <- 0;
    c.rotation <- 0

  let progress c =
    if c.enabled then begin
      c.perc <- c.perc + 1;
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
      Format.fprintf Format.err_formatter "%s%!" (Buffer.contents c.buffer)
    end

  let avalaible () = List.map (fun (n,_) -> n) !bars

end

module Timer = struct
  type t = {
    name: string;
    mutable count : int;
    mutable total : float;
    mutable last  : float;
    mutable is_in : bool;
  }

  let print ppf c =
    Format.fprintf ppf "Timer %s. Total time: %f. Count: %i@."
      c.name c.total c.count

  let create_verbose verbosity s =
    let c = { name = s; count = 0; total = 0.; last = 0.; is_in = false } in
    register verbosity (fun ppf -> print ppf c);
    c

  let create s = create_verbose Summary s

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

module Counter = struct
  type t = {
    name: string;
    mutable count : int;
  }

  let print ppf c =
    Format.fprintf ppf "Counter %s: %i@."
      c.name c.count

  let create s =
    let c = { name = s; count = 0 } in
    register Summary (fun ppf -> print ppf c);
    c

  let incr c =
    c.count <- c.count + 1

  let add c n =
    c.count <- c.count + n
end

(* we always print user / sys timers if the user ask to print Summary info *)
let print_process_time ppf =
  let pt = Unix.times() in
  Format.fprintf ppf "Process time (user):  %5.2f\n" pt.Unix.tms_utime;
  Format.fprintf ppf "Process time (sys):   %5.2f\n" pt.Unix.tms_stime
;;

(* XXX this does not work for some reason ... *)
register Summary (fun ppf -> print_process_time ppf) ;;

