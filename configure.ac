AC_INIT(dose3, 3.1.1, pietro.abate@pps.jussieu.fr)

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
 AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_CAMLP4
if test "$CAMLP4" = "no"; then
 AC_MSG_ERROR([You must install the Camlp4 pre-processor])
fi

if test "$CAMLP4OOPT" = "no" ; then
  CAMLP4OMACRO="$CAMLP4O Camlp4MacroParser.cmx"
else
  CAMLP4OMACRO="$CAMLP4OOPT -I_build Camlp4MacroParser.cmxs"
fi
AC_SUBST(CAMLP4O)
AC_SUBST(CAMLP4OMACRO)

AC_PROG_FINDLIB
AC_SUBST(OCAMLFIND)
if test "$OCAMLFIND" = "no"; then
 AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_ARG_WITH(bytecodeonly,
        [ --with-bytecodeonly ],
        OCAMLBEST=byte,
        OCAMLBEST="$OCAMLBEST")
if test "$OCAMLBEST" = "byte" ; then
  OCAMLOPT=
  AC_SUBST(OCAMLOPT)
fi

#defaults
USE_RPM=use_
LINK_RPM=link_
HAS_RPM=no
PKG_SQLITE=pkg_
PKG_PGSQL=pkg_
PKG_BZ2=pkg_
PKG_ZIP=pkg_
PKG_PARMAP=pkg_
PKG_XML=pkg_
PKG_CUDF=use_cudf
PKG_OCAMLGRAPH=pkg_
PKG_CURL=pkg_
PKG_OUNIT=pkg_
PKG_BENCHMARK=pkg_
DOC_OCAMLGRAPH=""
DOC_RPM=""

AC_ARG_WITH(libcudf,
        [ --without-libcudf ],
        HAS_LIBCUDF=no,
        HAS_LIBCUDF=yes)
AC_SUBST(HAS_LIBCUDF)
if test "$HAS_LIBCUDF" = "yes" ; then
  AC_CHECK_OCAML_PKG([cudf])
  if (test "$OCAML_PKG_cudf" = "no"); then
    AC_MSG_NOTICE([you asked to use an external libcudf, but ocamlfind cannot find it])
    AC_MSG_ERROR([Please specify --without-libcudf to compile cudf locally])
    PKG_CUDF=use_cudf
  else
    PKG_CUDF="package(cudf)"
  fi
else
  if test -f "cudf/cudf.mli" ; then
    AC_MSG_NOTICE([using local libcudf])
  else
    AC_MSG_ERROR([you asked to compile cudf locally, but the cudf submodule is not present])
  fi
fi

AC_CHECK_OCAML_PKG([extlib])
if test "$OCAML_PKG_extlib" = "no"; then
 AC_MSG_ERROR([Please install OCaml findlib module 'extlib'.])
fi

AC_CHECK_OCAML_PKG([pcre])
if test "$OCAML_PKG_pcre" = "no"; then
 AC_MSG_ERROR([Please install OCaml findlib module 'pcre'.])
fi

AC_ARG_WITH(parmap,
        [ --with-parmap ],
        HAS_PARMAP=yes,
        HAS_PARMAP=no)
AC_SUBST(HAS_PARMAP)
if test "$HAS_PARMAP" = "yes" ; then
  AC_CHECK_OCAML_PKG([parmap])
  if test "$OCAML_PKG_parmap" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'parmap'.])
  fi
  CONFIG_PARMAP="-DHASPARMAP"
  PKG_PARMAP="package(parmap)"
  PARMAP=parmap
fi


AC_ARG_WITH(zip,
        [ --with-zip ],
        HAS_ZIP=yes,
        HAS_ZIP=no)
AC_SUBST(HAS_ZIP)
if test "$HAS_ZIP" = "yes" ; then
  AC_CHECK_OCAML_PKG([zip])
  if test "$OCAML_PKG_zip" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'zip'.])
  fi
  CONFIG_ZIP="-DHASZIP"
  PKG_ZIP="package(zip)"
  ZIP=zip
fi

AC_ARG_WITH(bz2,
        [ --with-bz2 ],
        HAS_BZ2=yes,
        HAS_BZ2=no)
AC_SUBST(HAS_BZ2)
if test "$HAS_BZ2" = "yes" ; then
  AC_CHECK_OCAML_PKG([bz2])
  if test "$OCAML_PKG_bz2" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'bz2'.])
  fi
  CONFIG_BZ2="-DHASBZ2"
  PKG_BZ2="package(bz2)"
  BZ2=bz2
fi

AC_ARG_WITH(sqlite,
        [ --with-sqlite ],
        HAS_SQLITE=yes,
        HAS_SQLITE=no)
if test "$HAS_SQLITE" = "yes" ; then
  AC_CHECK_OCAML_PKG([sqlite3])
  if test "$OCAML_PKG_sqlite3" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'sqlite3'.])
  fi
  AC_CHECK_OCAML_PKG([json-static])
  if test "$OCAML_PKG_json-static" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'json-static'.])
  fi
  TAGS_SQLITE=sqlite
  PKG_SQLITE="pkg_sqlite3, pkg_json-static"
  MLPACK_SQLITE=Sqlite
  CONFIG_DBBACKEND="-DSQLITE"
  CONFIG_DB="-DHASDB"
fi

AC_ARG_WITH(postgresql,
        [ --with-postgresql ],
        HAS_PGSQL=yes,
        HAS_PGSQL=no)
if test "$HAS_PGSQL" = "yes" ; then
  AC_CHECK_OCAML_PKG([postgresql])
  if test "$OCAML_PKG_postgresql" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'postgresql'.])
  fi
  AC_CHECK_OCAML_PKG([json-static])
  if test "$OCAML_PKG_json-static" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'json-static'.])
  fi
  TAGS_PGSQL=pgsql
  PKG_PGSQL="pkg_postgresql, pkg_json-static, pkg_threads"
  MLPACK_PGSQL=Pgsql
  CONFIG_DBBACKEND="$CONFIG_DBBACKEND -DPGSQL"
  CONFIG_DB="-DHASDB"
fi

AC_ARG_WITH(ocamlgraph,
        [ --with-ocamlgraph ],
        HAS_OCAMLGRAPH=yes,
        HAS_OCAMLGRAPH=no)
AC_SUBST(HAS_OCAMLGRAPH)
if test "$HAS_OCAMLGRAPH" = "yes" ; then
  AC_CHECK_OCAML_PKG([ocamlgraph])
  if test "$OCAML_PKG_ocamlgraph" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'ocamlgraph'.])
  fi
  TMPVERSION=`$OCAMLFIND query -format %v ocamlgraph`
  PKG_OCAMLGRAPH="package(ocamlgraph)"
  MLPACK_OCAMLGRAPH="Defaultgraphs Statistics Dominators Strongdeps Strongdeps_int Strongconflicts Strongconflicts_int"
  DOC_OCAMLGRAPH="algo/Strongdeps algo/Strongconflicts algo/Strongdeps_int algo/Strongconflicts_int algo/Defaultgraphs algo/Statistics"
  CONFIG_OCAMLGRAPH="-DHASOCAMLGRAPH"
fi

AC_ARG_WITH(curl,
        [ --with-curl ],
        HAS_CURL=yes,
        HAS_CURL=no)
AC_SUBST(HAS_CURL)
if test "$HAS_CURL" = "yes" ; then
  AC_CHECK_OCAML_PKG([curl])
  if test "$OCAML_PKG_curl" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'curl'.])
  fi
  PKG_CURL="package(curl)"
fi

AC_ARG_WITH(xml,
        [ --with-xml ],
        HAS_XML=yes,
        HAS_XML=no)
AC_SUBST(HAS_XML)
if test "$HAS_XML" = "yes" ; then
  AC_CHECK_OCAML_PKG([expat])
  if test "$OCAML_PKG_expat" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'expat'.])
  fi
  AC_CHECK_OCAML_PKG([xml-light])
  if test "$OCAML_PKG_xml-light" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'xml-light'.])
  fi
  PKG_XML="package(expat), package(xml-light)"
fi

AC_ARG_WITH(oUnit,
        [ --with-oUnit ],
        HAS_OUNIT=yes,
        HAS_OUNIT=no)
AC_SUBST(HAS_OUNIT)
if test "$HAS_OUNIT" = "yes" ; then
  AC_CHECK_OCAML_PKG([oUnit])
  if test "$OCAML_PKG_oUnit" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'oUnit'.])
  fi
  PKG_OUNIT="pkg_oUnit"
fi

AC_ARG_WITH(benchmark,
        [ --with-benchmark ],
        HAS_BENCHMARK=yes,
        HAS_BENCHMARK=no)
AC_SUBST(HAS_BENCHMARK)
if test "$HAS_BENCHMARK" = "yes" ; then
  AC_CHECK_OCAML_PKG([benchmark])
  if test "$OCAML_PKG_benchmark" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'benchmark'.])
  fi
  PKG_BENCHMARK="pkg_benchmark"
fi

AC_HEADER_STDC

RPMPATH="-I/usr/include/rpm"
CFLAGS="$CFLAGS $RPMPATH"
CPPFLAGS="$CPPFLAGS $RPMPATH"

AC_ARG_WITH(rpm4,
        [ --with-rpm4 ],
        HAS_RPM4=yes,
        HAS_RPM4=no)
if test "$HAS_RPM4" = "yes" ; then
  AC_MSG_NOTICE([RPM support enabled])
  AC_CHECK_HEADERS([rpm/rpmio.h],,AC_MSG_ERROR([missing rpmio.h]))
  AC_CHECK_HEADERS([rpm/rpmtypes.h],,AC_MSG_ERROR([missing rpmtypes.h]))
  CONFIG_RPM="-DHASRPM"
  USE_RPM="use_rpm, use_rpm4"
  DOC_RPM="#rpm/Packages
  #rpm/Hdlists
  #rpm/Version
  #rpm/Rpmcudf"
  LINK_RPM="link_rpm4"
  HAS_RPM=yes
fi

AC_ARG_WITH(rpm5,
        [ --with-rpm5 ],
        HAS_RPM5=yes,
        HAS_RPM5=no)
if test "$HAS_RPM5" = "yes"; then
  AC_MSG_NOTICE([RPM 5 support enabled])
  CFLAGS="-I/usr/include/rpm"
  LDFLAG="-Lrpm -Lrpmio"
  AC_CHECK_HEADERS([rpm/rpmio.h],,AC_MSG_ERROR([missing rpmio.h]))
#  AC_CHECK_HEADERS([rpm/rpmgi.h],,AC_MSG_ERROR([missing rpmgi.h]))
#  AC_CHECK_HEADERS([rpm/rpm46compat.h],,AC_MSG_ERROR([missing rpm46compat.h]))
#  AC_CHECK_HEADERS([rpm/rpm4compat.h],,AC_MSG_ERROR([missing rpm4compat.h]))
  CONFIG_RPM="-DHASRPM"
  USE_RPM="use_rpm, use_rpm5"
  DOC_RPM="#rpm/Packages
  #rpm/Hdlists
  #rpm/Version
  #rpm/Rpmcudf"
  LINK_RPM="link_rpm5"
  HAS_RPM=yes
fi

#AC_CHECK_HEADERS([sched.h],,AC_MSG_ERROR([missing sched.h]))
#AC_CHECK_DECLS([sched_setaffinity], [], [], [[
#                #define _GNU_SOURCE 1
#                #include <sched.h>
#                ]])



AC_SUBST(TAGS_PGSQL)
AC_SUBST(PKG_PGSQL)
AC_SUBST(MLPACK_PGSQL)
AC_SUBST(PKG_PGSQL)

AC_SUBST(TAGS_SQLITE)
AC_SUBST(PKG_SQLITE)
AC_SUBST(MLPACK_SQLITE)
AC_SUBST(PKG_SQLITE)

AC_SUBST(CONFIG_DBBACKEND)
AC_SUBST(CONFIG_DB)

AC_SUBST(CONFIG_ZIP)
AC_SUBST(PKG_ZIP)
AC_SUBST(ZIP)

AC_SUBST(CONFIG_PARMAP)
AC_SUBST(PKG_PARMAP)
AC_SUBST(PARMAP)

AC_SUBST(CONFIG_BZ2)
AC_SUBST(PKG_BZ2)
AC_SUBST(BZ2)

AC_SUBST(LINK_RPM)
AC_SUBST(CONFIG_RPM)
AC_SUBST(INCLUDE_RPM)
AC_SUBST(USE_RPM)
AC_SUBST(LINK_RPM)
AC_SUBST(HAS_RPM)
AC_SUBST(DOC_RPM)

AC_SUBST(INCLUDE_CUDF)
AC_SUBST(PKG_CUDF)

if (test "$HAS_SQLITE" = "yes") || (test "$HAS_PGSQL" = "yes") then
  HAS_DB=yes
else
  HAS_DB=no
fi
AC_SUBST(HAS_DB)

AC_SUBST(MLPACK_OCAMLGRAPH)
AC_SUBST(DOC_OCAMLGRAPH)
AC_SUBST(PKG_OCAMLGRAPH)
AC_SUBST(CONFIG_OCAMLGRAPH)

AC_SUBST(PKG_CURL)

AC_SUBST(PKG_XML)

LDFLAGS="-fstack-protector"

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)

AC_ARG_WITH(experimental,
        [ --with-xperimental ],
        BUILD_EXP=yes,
        BUILD_EXP=no)
if test "$BUILD_EXP" = "yes" ; then
  AC_CONFIG_FILES([
    experimental/Makefile
  ])
fi

AC_CONFIG_FILES([
  Makefile.config
  _tags
  META
  dose3.odocl
  db/db.mlpack
  algo/algo.mlpack
  common/versionInfo.ml
])
AC_OUTPUT

