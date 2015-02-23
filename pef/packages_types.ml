type name = string
type version = string
type architecture = string
type vpkgname = (string * architecture option)
type relop = string
type constr = (relop * version)

type vpkg = (vpkgname * constr option)
type vpkglist = vpkg list
type vpkgformula = vpkg list list
