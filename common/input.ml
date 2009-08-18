
open ExtLib

let wrap f = try f () with End_of_file -> raise IO.No_more_input

#ifdef HASZIP
let gzip_open_file file =
  let ch = Gzip.open_in file in
  IO.create_in
  ~read:(fun () -> wrap (fun _ -> Gzip.input_char ch))
  ~input:(wrap (fun _ -> Gzip.input ch))
  ~close:(fun () -> Gzip.close_in ch)
;;
#endif

#ifdef HASBZ2
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
#endif

let std_open_file file = IO.input_channel (open_in file)
let open_ch ch = IO.input_channel ch
let close_ch ch = IO.close_in ch

let open_file file =
#ifdef HASZIP
  if Filename.check_suffix file ".gz" then
    gzip_open_file file
  else 
#endif
#ifdef HASBZ2
  if Filename.check_suffix file ".bz2" then
    bzip_open_file file
  else
#endif
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
