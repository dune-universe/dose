(***************************************************************************************)
(*  Copyright (C) 2009  Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*                                                                                     *)
(*  This library is free software: you can redistribute it and/or modify               *)
(*  it under the terms of the GNU Lesser General Public License as                     *)
(*  published by the Free Software Foundation, either version 3 of the                 *)
(*  License, or (at your option) any later version.  A special linking                 *)
(*  exception to the GNU Lesser General Public License applies to this                 *)
(*  library, see the COPYING file for more information.                                *)
(***************************************************************************************)

(** 
In order to find a solution we make the following assumptions/restrictions:

{ol 
{- A package that is installed cannot be removed, but it can be replaced:
  {ul
    {- A package that is installed can be replaced by the same package with a
    more recent version.}
    {- A package A can be replaced by a different package B that offers the same
    functionalities specified via provides.}
  }
}
{- If a package is upgraded there is a preference on the most recent version.}
{- No Downgrade}
}

The first assumption would be like to specify "Keep: feature" in the cudf
document for all installed packages.

These assumptions are conservative and restrict the space of solutions
indentified by the cudf semantics. A real cudf_installer should allow all
solutions and at the same time use different optimization criterias to select
only solutions that match these criterias.

We encode the installation problem as follows:

pkg: a
depends : b, c | d
- {v -a v [b] v}
- {v -a v [c] v [d] v}

--------------------------

pkg: a
conflicts: c, d
- {v -a v [-c] v}
- {v -a v [-d] v}

--------------------------

pkg: a
installed: true
- {v -request v [a] v}

--------------------------

The first two clauses correspond to the edos econding.  The function [_] :
(pkg,constraint) -> pkg list is the expansion function that for a package and a
contraint returns the list of all versions of this package matching this
constraint. The expansion function is also provides aware.

The last clause encodes the fact that if a package is installed, then it can be
eventually replaced by another package that respects the assumption n. 1

We encode requests as follows:

Install: aa
- {v -request v [a] v}

This condition simply states that in order to satisfy the request at least
an alternative for [a] must be true.


Upgrade: bb
- {v -request v [a] v}
- {v -request v [-a] v}

This conditions implies that only one alternative for [a] must be installed.

Remove: cc
- for all a' in [a]
- {v -request v -a' v}

This condition states that in order to fulfill the request none of the
alternatives for [a] can be installed. In order to satisfy this constraint we
also remove all occurrences of a' \in [a] from the set of alternatives of
installed packages and packages to install and to upgrade.
*)

type solver

(** load the solver cudf problem and create an instance of the solver *)
val load : Cudf.universe -> Cudf.request -> solver

(** run the solver *)
val solve : solver -> Diagnostic.diagnosis
