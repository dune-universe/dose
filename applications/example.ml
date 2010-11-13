(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  ADD authors here                                     *)
(*                                                                        *)
(*  Contributor(s):  ADD minor contributors here                          *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

open ExtLib
open Common

let info fmt = Util.make_info "test" fmt
let warning fmt = Util.make_warning "test" fmt
let debug fmt = Util.make_debug "test" fmt
let fatal fmt = Util.make_fatal "test" fmt

module Options = struct
  open OptParse
  let options = OptParser.make ~description:"add a decription here"
  include Boilerplate.MakeOptions(struct let options = options end)

  let fail = StdOpt.store_true ()
  open OptParser
  add options ~short_name:'f' ~long_name:"fail" ~help:"exit with a failoure" fail;
end

let main () =

  let args = OptParse.OptParser.parse_argv Options.options in
  
  (* enable info / warning / debug information *)
  Boilerplate.enable_debug (OptParse.Opt.get Options.verbose);
  
  (* enable a selection of progress bars *)
  Boilerplate.enable_bars (OptParse.Opt.get Options.progress) [] ;

  (* enable a selection of timers *)
  Boilerplate.enable_timers (OptParse.Opt.get Options.timers) [];

  info "print some verbose info";
  warning "print some warnings";
  debug "print some debug";
  
  List.iter (Printf.printf "Arg : %s\n") args;
  
  if OptParse.Opt.get Options.fail then
    fatal "this is a fatal error"
  ;

  (* add here the rest *)
;;

main ();;

