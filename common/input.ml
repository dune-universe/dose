
open ExtLib

let wrap f = try f () with End_of_file -> raise IO.No_more_input

IFDEF HASZIP THEN
let gzip_open_file file =
  let ch = Gzip.open_in file in
  IO.create_in
  ~read:(fun () -> wrap (fun _ -> Gzip.input_char ch))
  ~input:(wrap (fun _ -> Gzip.input ch))
  ~close:(fun () -> Gzip.close_in ch)
;;
END

IFDEF HASBZ2 THEN
(*
 * Almost there , but not quite
let bzip_open_in file =
  let ch = Bz2.open_in file in
  IO.create_in
  ~read:(fun () -> Bz2.input_char ch)
  ~input:(Gzip.input ch)
  ~close:(fun () -> Bz2.close_in ch)
*)

let bzip_open_file file = failwith "Not Yet implemented"
END

let std_open_file file = IO.input_channel (open_in file)
let open_ch ch = IO.input_channel ch
let close_ch ch = IO.close_in ch

let open_file file =
  if Filename.check_suffix file ".gz" then
IFDEF HASZIP THEN
    gzip_open_file file
ELSE
    failwith "gzip not supported"
END
  else 
  if Filename.check_suffix file ".bz2" then
IFDEF HASBZ2 THEN
    bzip_open_file file
ELSE
    failwith "bzip not supported"
END
  else 
    std_open_file file
;;

let parse_uri = Uri.parseUri

(*******************************)

(* XXX stubs *)
(*let parseUri uri =
  ("pgsql", (Some("test"), Some("tester"), Some("localhost"), None, "debian"))
let query = (`Interval ("2007-01-01", "2007-01-02"))
*)
(*******************************)
