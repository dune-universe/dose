opam init -y
eval `opam config env`
opam switch -y 4.02.1
opam install -y extlib camlbz2 camlzip ocamlgraph extlib ounit re cudf
