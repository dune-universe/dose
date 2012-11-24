(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
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

include Util.Logging(struct let label = __FILE__ end) ;;

module type Ot = sig
  val options :
    ?usage:string ->
    ?version:string ->
    ?suppress_usage:bool ->
    ?suppress_help:bool ->
    ?prog:string ->
    ?formatter:OptParse.Formatter.t -> unit -> OptParse.OptParser.t
end

(* *************************************** *)

let vpkg_option ?default ?(metavar = "VPKG") () =
  let parse_vpkg s = 
    let _loc = Debian.Format822.dummy_loc in
    Debian.Packages.parse_vpkg (_loc,s)
  in
  OptParse.Opt.value_option metavar default
  parse_vpkg (fun _ s -> Printf.sprintf "invalid vpackage '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (<c> v)" *)
let vpkglist_option ?default ?(metavar = "VPKGLST") () =
  let parse_vpkglist s = 
    let _loc = Debian.Format822.dummy_loc in
    Debian.Packages.parse_vpkglist (_loc,s)
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "invalid vpackage list '%s'" s)
;;

(* this is a ,-separated list of vpkgs of the form "a (= v)" *)
let pkglist_option ?default ?(metavar = "PKGLST") () =
  let parse_vpkglist s = 
    let _loc = Debian.Format822.dummy_loc in
    List.map (function
      |((n,a),Some("=",v)) -> (n,a,v)
      |((n,a),None) ->
          raise (Debian.Packages.ParseError (s,"you must specify a version" ))
      |_ -> raise (Debian.Packages.ParseError (s,""))
    ) (Debian.Packages.parse_vpkglist (_loc,s))
  in
  OptParse.Opt.value_option metavar default
  parse_vpkglist (fun _ s -> Printf.sprintf "invalid package list '%s'" s)
;;

(* *************************************** *)

let incr_str_list ?(default=Some []) ?(metavar = "STR") =
  let acc = ref [] in 
  let coerce s = acc := s :: !acc ; !acc in
  fun () ->
  OptParse.Opt.value_option metavar default coerce 
  (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* this is a ,-separated list of strings *)
let str_list_option ?default ?(metavar = "STRLST") =
  let sep = "," in
  let coerce s = ExtString.String.nsplit s sep in
  fun () ->
    OptParse.Opt.value_option metavar default coerce
    (fun _ s -> Printf.sprintf "Invalid String '%s'" s)
;;

(* *************************************** *)

module MakeOptions(O : Ot) = struct
  open OptParse ;;

  let verbose = StdOpt.incr_option ()
  let quiet = StdOpt.store_true ()
  let progress = StdOpt.store_true ()
  let timers = StdOpt.store_true ()
  let options = O.options ~version:VersionInfo.version () ;;

  open OptParser ;;
  add options ~short_name:'v' ~long_name:"verbose" ~help:"print additional information" verbose;
  add options ~long_name:"progress" ~help:"print progress bars" progress;
  add options ~long_name:"timers" ~help:"print timing information" timers;
  add options ~long_name:"quiet" ~help:"do no print any messages" quiet;

end

type options =
  |Deb of Debian.Debcudf.options
  |Eclipse of Debian.Debcudf.options
  |Edsp of Debian.Debcudf.options
  |Csw
  |Rpm
  |Cudf

module MakeDistribOptions(O : sig val options : OptParse.OptParser.t end) = struct
  open OptParse ;;

  let deb_native_arch = StdOpt.str_option ()
  let deb_foreign_archs = str_list_option ()
  let deb_host_arch = StdOpt.str_option ()
  let deb_ignore_essential = StdOpt.store_true ()
  let default_deb_options = ref 
    ["deb-native-arch";
     "deb-host-arch";
     "deb-foreign-archs";
     "deb-ignore-essential"]

  let remove_deb_option o =
    default_deb_options := List.remove !default_deb_options o

  let set_deb_options () =
    let native =
      if Opt.is_set deb_native_arch then
        Opt.get deb_native_arch
      else ""
    in
    let host =
      if Opt.is_set deb_host_arch then begin
        (* if host arch is set, native arch must be set *)
        if native = "" then
          fatal "you must specify at least the native architecture" ;
        Opt.get deb_host_arch
      end
      else native
    in
    let foreign =
      (* if host arch is set, it is an implicit foreign arch *)
      if Opt.is_set deb_foreign_archs then begin
        let f = Opt.get deb_foreign_archs in
        if Opt.is_set deb_host_arch then
          host::f
        else
          f
      end else begin
        if Opt.is_set deb_host_arch then
          [host]
        else
          []
      end
    in
    {
      Debian.Debcudf.default_options with
      Debian.Debcudf.native = native;
      foreign = foreign;
      host = host;
      ignore_essential = Opt.get deb_ignore_essential
    }
  ;;

  let set_default_options = function
    |`Deb -> Some (
      Deb { 
        Debian.Debcudf.default_options with
        Debian.Debcudf.ignore_essential = true
      })
    |`Edsp -> Some (
      Edsp { 
        Debian.Debcudf.default_options with
        Debian.Debcudf.ignore_essential = true
      })
    |`Eclipse -> Some (Eclipse Debian.Debcudf.default_options)
    |_ -> None

  let set_options = function
    |`Deb -> Some (Deb (set_deb_options ()))
    |`Edsp -> Some (Edsp (set_deb_options ()))
    |`Eclipse -> Some (Eclipse Debian.Debcudf.default_options)
    |_ -> None
  ;;

  open OptParser ;;
  if List.length !default_deb_options > 0 then begin
    let deb_group = add_group O.options "Debian Specific Options" in
    if List.mem "deb-native-arch" !default_deb_options then
      add O.options ~group:deb_group ~long_name:"deb-native-arch"
      ~help:"Native architecture" deb_native_arch;
    if List.mem "deb-host-arch" !default_deb_options then
      add O.options ~group:deb_group ~long_name:"deb-host-arch" 
      ~help:"Native/cross compile host architecture, defaults to native architecture" deb_host_arch;
    if List.mem "deb-foreign-archs" !default_deb_options then
      add O.options ~group:deb_group ~long_name:"deb-foreign-archs" 
      ~help:"Foreign architectures in addition to native and host architectures" deb_foreign_archs;
    if List.mem "deb-ignore-essential" !default_deb_options then
      add O.options ~group:deb_group ~long_name:"deb-ignore-essential" 
      ~help:"Ignore Essential Packages" deb_ignore_essential;
  end

(*  let rpm_group = add_group options "Rpm Specific Options" in
    let eclipse_group = add_group options "Eclipse Specific Options" in
*)
end


