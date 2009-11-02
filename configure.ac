
AC_INIT(libmancoosi, 0.1, pietro.abate@pps.jussieu.fr)

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
 AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
 AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_CHECK_OCAML_PKG([extlib])
if test "$OCAML_PKG_extlib" = "no"; then
 AC_MSG_ERROR([Please install OCaml findlib module 'extlib'.])
fi

AC_CHECK_OCAML_PKG([cudf])

AC_ARG_WITH(localcudf,
        [ --with-localcudf ],
        HAS_LOCALCUDF=yes,
        HAS_LOCALCUDF=no)
if test "$HAS_LOCALCUDF" = "yes" ; then
  AC_MSG_NOTICE([using local libcudf])
  INCLUDE_CUDF="<libcudf>: include"
  PKG_CUDF=pkg_cudf
else
  PKG_CUDF=pkg_
fi

if (test "$OCAML_PKG_cudf" = "no") && (test "$HAS_LOCALCUDF" = "no"); then
 AC_MSG_ERROR([Please install OCaml findlib module 'cudf'.])
fi

AC_ARG_WITH(zip,
        [ --with-zip ],
        HAS_ZIP=yes,
        HAS_ZIP=no)
AC_SUBST(HAS_ZIP)
if test "$HAS_ZIP" = "yes" ; then
  AC_MSG_NOTICE([zip support enabled])
  AC_CHECK_OCAML_PKG([zip])
  if test "$OCAML_PKG_zip" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'zip'.])
  fi
  CONFIG_ZIP="-DHASZIP"
  PKG_ZIP=pkg_zip
else
  PKG_ZIP=pkg_
fi

#AC_CHECK_OCAML_PKG([bz2])
PKG_BZ2=pkg_

AC_ARG_WITH(rpm,
        [ --with-rpm ],
        HAS_RPM=yes,
        HAS_RPM=no)
AC_SUBST(HAS_RPM)
if test "$HAS_RPM" = "yes" ; then
  AC_MSG_NOTICE([RPM support enabled])
  AC_CHECK_HEADERS([rpm/rpmlib.h],,AC_MSG_ERROR([missing rpmlib.h]))
  AC_CHECK_HEADERS([rpm/rpmio.h],,AC_MSG_ERROR([missing rpmlib.h]))
  INCLUDE_RPM="<rpm>: include"
fi

AC_ARG_WITH(sqlite,
        [ --with-sqlite ],
        HAS_SQLITE=yes,
        HAS_SQLITE=no)
if test "$HAS_SQLITE" = "yes" ; then
  AC_MSG_NOTICE([sqlite3 support enabled])
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
else
  PKG_SQLITE=pkg_
fi

AC_ARG_WITH(postgresql,
        [ --with-postgresql ],
        HAS_PGSQL=yes,
        HAS_PGSQL=no)
if test "$HAS_PGSQL" = "yes" ; then
  AC_MSG_NOTICE([postgresql installation enabled])
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
else
  PKG_PGSQL=pkg_
fi

AC_ARG_WITH(ocamlgraph,
        [ --with-ocamlgraph ],
        HAS_OCAMLGRAPH=yes,
        HAS_OCAMLGRAPH=no)
AC_SUBST(HAS_OCAMLGRAPH)
if test "$HAS_OCAMLGRAPH" = "yes" ; then
  AC_MSG_NOTICE([ocamlgraph installation enabled])
  AC_CHECK_OCAML_PKG([ocamlgraph])
  if test "$OCAML_PKG_ocamlgraph" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'ocamlgraph'.])
  fi
  PKG_OCAMLGRAPH=pkg_ocamlgraph
  MLPACK_OCAMLGRAPH="Defaultgraphs Statistics Dominators Strongdeps Strongdeps_int"
else
  PKG_OCAMLGRAPH=pkg_
fi

AC_ARG_WITH(xml,
        [ --with-xml ],
        HAS_XML=yes,
        HAS_XML=no)
AC_SUBST(HAS_XML)
if test "$HAS_XML" = "yes" ; then
  AC_MSG_NOTICE([expat installation enabled])
  AC_CHECK_OCAML_PKG([expat])
  if test "$OCAML_PKG_expat" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'expat'.])
  fi
  AC_CHECK_OCAML_PKG([xml-light])
  if test "$OCAML_PKG_xml-light" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'xml-light'.])
  fi
  PKG_XML="pkg_expat, pkg_xml-light"
else
  PKG_XML=pkg_
fi

AC_ARG_WITH(oUnit,
        [ --with-oUnit ],
        HAS_OUNIT=yes,
        HAS_OUNIT=no)
AC_SUBST(HAS_OUNIT)
if test "$HAS_OUNIT" = "yes" ; then
  AC_MSG_NOTICE([oUnit installation enabled])
  AC_CHECK_OCAML_PKG([oUnit])
  if test "$OCAML_PKG_oUnit" = "no"; then
   AC_MSG_ERROR([Please install OCaml findlib module 'oUnit'.])
  fi
  PKG_OUNIT="pkg_oUnit"
else
  PKG_OUNIT=pkg_
fi

AC_SUBST(TAGS_PGSQL)
AC_SUBST(PKG_PGSQL)
AC_SUBST(MLPACK_PGSQL)
AC_SUBST(PKG_PGSQL)

AC_SUBST(TAGS_SQLITE)
AC_SUBST(PKG_SQLITE)
AC_SUBST(MLPACK_SQLITE)
AC_SUBST(PKG_SQLITE)

AC_SUBST(CONFIG_DBBACKEND)

AC_SUBST(CONFIG_ZIP)
AC_SUBST(PKG_ZIP)

AC_SUBST(CONFIG_BZ2)
AC_SUBST(PKG_BZ2)

AC_SUBST(INCLUDE_RPM)
AC_SUBST(INCLUDE_CUDF)

if (test "$HAS_SQLITE" = "yes") || (test "$HAS_PGSQL" = "yes") then
  HAS_DB=yes
else
  HAS_DB=no
fi
AC_SUBST(HAS_DB)

AC_SUBST(MLPACK_OCAMLGRAPH)
AC_SUBST(PKG_OCAMLGRAPH)

AC_SUBST(PKG_XML)

AC_SUBST(CFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)

AC_CONFIG_FILES([
  Makefile.config
  db/db.mlpack
  db/_tags
  algo/_tags
  rpm/_tags
  deb/_tags
  algo/algo.mlpack
  common/_tags
  applications/_tags
  _tags
])
AC_OUTPUT

