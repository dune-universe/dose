#!/bin/sh

echo "Cleaning up before"
for i in deb common opam pef algo versioning ; do
  F=$(ls $i/*.mlpack) ;
  rm -f ${F%.mlpack}.ml
  rm -f ${F%.mlpack}.mli
done

echo "Create packs"
for i in deb common opam pef algo versioning ; do
  echo -n "$i..."
  F=$(ls $i/*.mlpack) ;
  ML=$(cat $F | sed -e "s/\(.\)\(.*\)/_build\/$i\/\L\1\E\2.ml/" |  tr '\n' ' ') ;
  MML=
  for f in $ML; do
    if echo $f | grep -v -E "_lexer|_parser" ; then
      MML="$MML $f"
    fi
  done > /dev/null
  scripts/ocp-pack -no-ml -mli -pp 'cppo -D '\''OCAMLGRAPHVERSION 186'\''' -o ${F%.mlpack}.ml $MML ;
done

i=doseparse
echo "$i..."
F=$(ls $i/doseparse.mlpack) ;
ML=$(cat $F | sed -e "s/\(.\)\(.*\)/_build\/$i\/\L\1\E\2.ml/" |  tr '\n' ' ') ;
MML=
for f in $ML; do
  if echo $f | grep -v -E "_lexer|_parser" ; then
    MML="$MML $f"
  fi
done > /dev/null
scripts/ocp-pack -pp 'cppo -D '\''OCAMLGRAPHVERSION 186'\''' -o ${F%.mlpack}.ml $MML ;

echo 

ocamlbuild dose3.docdir/index.html dose3.docdir/index.dot

echo "Cleaning up after"
for i in deb common opam pef algo versioning ; do
  F=$(ls $i/*.mlpack) ;
  rm -f ${F%.mlpack}.ml
  rm -f ${F%.mlpack}.mli
done
