(**************************************************************************************)
(*  Copyright (C) 2009-2015 Pietro Abate <pietro.abate@pps.univ-paris-diderot.fr>     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)
 
(** this functions follow the semantic versioning specification http://semver.org/ *)

(** A prerelease can have strings or numbers *)
type identifier =
  | NumericIdentifier of int
  | StringIdentifier of string

type version = {
  major: int;
  minor: int;
  patch: int;
  pre: identifier list; (** A list of dot splited elements *)
  build: string list;
}

(** Parses a string into a version. If full is false a more forgiving
    regular expression is used for parsing.
    Raise Invalid_argument if the version can not be parsed *)
val parse_version : bool -> string -> version

val parse_version_option : bool -> string -> version option

val compare_version : version -> version -> int


(** Compare two strings intepreted as semantic versions. 
    If full is false a more forgiving regular expression is used for parsing. 
    Raise Invalid_argument if one of the versions can not be parsed *)
val parse_and_compare : bool -> string -> string -> int

(** like [parse_and_compare] always using the full parser *)
val compare : bool -> string -> string -> int

(** Equality between two versions *)
val equal : bool -> string -> string -> bool
