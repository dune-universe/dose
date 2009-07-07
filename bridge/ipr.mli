(*****************************************************************************)
(*  Copyright (C) 2009  <pietro.abate@pps.jussieu.fr>                        *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(** IPR : Intermediate package representation *)
(** This module provides a standard data structure to parse textual packages 
    description files. This format is purely sintactic and does not encode any 
    specific semanctic information related to the input format *)

module TF : functor (Cudf : Cudf.T) ->
  sig
    module type T = 
      sig

        module Cudf_printer : Cudf_printer.TF(Cudf).T
        module Cudf_parser : Cudf_parser.TF(Cudf).T
        module Cudf_checker : Cudf_checker.TF(Cudf).T

        type name = string
        type version = string
        type vpkg = string * (string * string) option
        type veqpkg = string * (string * string) option

        (** Representation of a parsed package description item. 
            all fields are string *)
        type package = {
          name : name;
          version : version;
          depends : vpkg list list;
          pre_depends : vpkg list list;
          recommends : vpkg list;
          suggests : vpkg list;
          enhances : vpkg list;
          conflicts : vpkg list;
          replaces : vpkg list;
          provides : veqpkg list;
        }

        val default_package : package

        (** a commodity Set of Ipr packages. Commonly used to remove duplicates. *)
        module Set : Set.S with type elt = package

        (** initialize the version conversion tables 
            @param cmp : version comparison function *)
        val init_tables : ?cmp:(version -> version -> int) -> package list -> unit

        (** return a cudf version number accordingly to the conversions tables *)
        (* val get_version : name * version -> int *)

        (** convert the a package in the ipr format to cudf. The resulting
            cudf package will be obtained only by mapping versions (string) 
            to integers using the concersion tables initialized with the function
            [init_tables]. *)
        val tocudf : ?inst:bool -> package -> Cudf.package

        (** escape character not conforming to the cudf spec *)
        val escape : string -> string

        (** add an extra field to the Cudf package.
            the filed must be declared in advance. Supported extra fields are
            - number : string representation of the package version
            - priority : package priority (used by apt)
            - release : release to which the package belongs to *)
        val add_extra : string * Cudf.Extra.t -> Cudf.package -> Cudf.package

        (** load a Cudf universe.
            @param init: skip version table initilization
            @param cmp: version comparison function *)
        val load_universe :
          ?init:bool ->
          ?cmp:(version -> version -> int) -> package list -> Cudf.universe
      end
  end

module Make : functor (Cudf : Cudf.T) -> TF(Cudf).T
(** by default with support plain Cudf *)

module ExtraDefault : Cudf.Extra
with type t = [
    |`Version of string
    |`Release of string
    |`Priority of int
    |`UnParsed of string
  ]

module Cudf : Cudf.T with module Extra = ExtraDefault
include TF(Cudf).T
