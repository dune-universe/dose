(**************************************************************************************)
(*  Copyright (C) 2015 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2015 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Opam/Pef format conversion routines *)

open ExtLib
open Common

#define __label __FILE__ 
let label =  __label ;;
include Util.Logging(struct let label = label end) ;;

