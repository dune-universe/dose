

let compare_versions = SemverNode.compare_version
let compare x y = SemverNode.parse_and_compare true x y
let equal = SemverNode.equal true
