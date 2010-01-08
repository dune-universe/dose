open ExtLib
open Common

let argv_ f l =
  let a = Array.of_list l in
  if Array.length a = 0 then
    (Printf.eprintf "No input file specified" ; exit 1)
  else f a

let argv1 l = argv_ (fun a -> a.(0)) l
let argv2 l = argv_ (fun a -> (a.(0),a.(1))) l
let argv3 l = argv_ (fun a -> (a.(0),a.(1),a.(2))) l

let load_universe uri =
  Util.print_info "Parsing and normalizing..." ;
  let timer = Util.Timer.create "Parsing and normalizing" in
  Util.Timer.start timer;
  let universe =
    match Input.parse_uri uri with
    |(("pgsql"|"sqlite") as dbtype,info,(Some query)) ->
IFDEF HASDB THEN
      begin
        let db = Db.Backend.init_database dbtype info (Idbr.parse_query query) in
        let l = Db.Backend.load_selection db (`All) in
        Debian.Debcudf.load_universe l
      end
ELSE
      failwith (dbtype^" Not supported")
END
    |("deb",(_,_,_,_,"-"),_) -> begin
      let l = Debian.Packages.input_raw_ch (IO.input_channel stdin) in
      Debian.Debcudf.load_universe l
    end
    |("deb",(_,_,_,_,file),_) -> begin
      let l = Debian.Packages.input_raw [file] in
      Debian.Debcudf.load_universe l
    end
    |("cudf",(_,_,_,_,file),_) -> begin
      let _, u, _ = CudfAdd.load_cudf file in u
    end
    |("hdlist",(_,_,_,_,file),_) ->
IFDEF HASRPM THEN
    begin
      let l = Rpm.Packages.Hdlists.input_raw [file] in
      Rpm.Rpmcudf.load_universe l
    end
ELSE
    failwith ("hdlist Not supported")
END
    |("synth",(_,_,_,_,file),_) ->
IFDEF HASRPM THEN
    begin
      let l = Rpm.Packages.Synthesis.input_raw [file] in
      Rpm.Rpmcudf.load_universe l
    end
ELSE
    failwith ("synth Not supported")
END
    |(s,_,_) -> failwith (s^" Not supported")
  in
  ignore(Util.Timer.stop timer ());
  Util.print_info "done" ;
  universe
;;

