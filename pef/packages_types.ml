type name = string
type version = string
type architecture = string
type architectures = architecture list
type buildprofile = string
type vpkgname = (string * architecture option)
type multiarch = [ `Foreign | `Allowed | `No | `Same ]
type source = (name * version option)
type relop = string
type constr = (relop * version)
type installed = bool

(*type vpkg = (vpkgname * constr option)*)
type vpkg =
  |Name of name
  |NameArch of (name * architecture)
  |NameConstr of (name * constr)
  |NameArchConstr of (name * architecture * constr)

type vpkglist = vpkg list
type vpkgformula = vpkg list list

type builddep = (vpkg * (bool * architecture) list * (bool * buildprofile) list list)
type builddepslist = builddep list
type builddepsformula = builddep list list

type action = I | R
type suite = string
type vpkgreq = (action option * vpkg * suite option)

let make_vpkg = function
  |((n,None),None) -> Name n
  |((n,Some a),None) -> NameArch(n,a)
  |((n,None),Some constr) -> NameConstr (n,constr)
  |((n,Some a),Some constr) -> NameArchConstr (n,a,constr)

let _compatiblity_vpkg_filter = function
  |Name n -> ((n,None),None)
  |NameArch (n,a) -> ((n,Some a),None)
  |NameArchConstr (n,a,c) -> ((n,Some a),Some c)
  |NameConstr (n,c) -> ((n,None),Some c)

