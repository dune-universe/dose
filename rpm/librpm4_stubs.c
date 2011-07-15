/**************************************************************************************/
/*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     */
/*  Copyright (C) 2009 Mancoosi Project                                               */
/*                                                                                    */
/*  This library is free software: you can redistribute it and/or modify              */
/*  it under the terms of the GNU Lesser General Public License as                    */
/*  published by the Free Software Foundation, either version 3 of the                */
/*  License, or (at your option) any later version.  A special linking                */
/*  exception to the GNU Lesser General Public License applies to this                */
/*  library, see the COPYING file for more information.                               */
/**************************************************************************************/

#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include <rpm/rpmtypes.h>
#include <rpm/rpmlib.h>
#include <rpmtag.h>
#include <rpm/header.h>
#include <rpm/rpmfi.h>
#include <rpm/rpmts.h>

#define Val_none Val_int(0)

void raise_Eof () {
  static value * exn = NULL;
  if (exn == NULL)
      exn = caml_named_value ("hdlist.eof");
  raise_constant (*exn);
}

#define VARIANT_S 0
#define VARIANT_L 1
#define VARIANT_D 2

static inline value Val_some( value v ) {
  CAMLparam1( v );
  CAMLlocal1( some );
  some = caml_alloc(1, 0);
  Store_field( some, 0, v );
  CAMLreturn(some);
}

static inline value tuple( value a, value b) {
  CAMLparam2( a, b );
  CAMLlocal1( tuple );

  tuple = caml_alloc_tuple(2);

  Store_field( tuple, 0, a );
  Store_field( tuple, 1, b );

  CAMLreturn(tuple);
}

static inline value append( value hd, value tl ) {
  CAMLparam2( hd , tl );
  CAMLreturn(tuple( hd, tl ));
}

static inline value string_variant(value s) {
  CAMLparam1( s );
  CAMLlocal1( v );
  v = caml_alloc(1, VARIANT_S);
  Store_field(v, 0, s);
  CAMLreturn(v);
}

static inline value list_variant_L(value l) {
  CAMLparam1( l );
  CAMLlocal1( v );
  v = caml_alloc(1, VARIANT_L);
  Store_field(v, 0, l);
  CAMLreturn(v);
}

static inline value list_variant_D(value l) {
  CAMLparam1( l );
  CAMLlocal1( v );
  v = caml_alloc(1, VARIANT_D);
  Store_field(v, 0, l);
  CAMLreturn(v);
}

value get_deps(Header h, rpmTag tag) {
  CAMLparam0 ();
  CAMLlocal2( hd, tl );
  CAMLlocal1( constr );
  tl = Val_emptylist;
  constr = Val_none;
  const char *name, *version;
  rpmsenseFlags flag;

  rpmds deps;
  deps = rpmdsNew(h, tag, 0);
  while (rpmdsNext(deps) != -1) {
    printf(" %s\n",rpmdsDNEVR(deps));
    constr = Val_none;

    flag = rpmdsFlags(deps);
    if (!(flag & RPMSENSE_RPMLIB)) {
      name = rpmdsN(deps);
      if ((flag & RPMSENSE_EQUAL) ||
          (flag & RPMSENSE_LESS) ||
          (flag & RPMSENSE_GREATER)) {
        if ((version = rpmdsEVR(deps)) != NULL) {
          printf("%s %d %s\n",name,flag,version);
          constr = Val_some(tuple(caml_copy_int32(flag),caml_copy_string(version)));
        }
      }
      hd = tuple(caml_copy_string(name),constr);
      tl = append(hd,tl);
    }
  };
  rpmdsFree(deps);

  CAMLreturn(list_variant_D(tl));
}

value get_filedeps(Header h) {
  CAMLparam0 ();
  const char *fname;
  CAMLlocal2( hd, tl );
  tl = Val_emptylist;
  rpmts ts = rpmtsCreate();
  rpmfi fi = rpmfiNew(ts, h, RPMTAG_BASENAMES, RPMFI_NOHEADER);
  while (rpmfiNext(fi) != -1) {
    fname = rpmfiFN(fi);
    hd = caml_copy_string(fname);
    tl = append(hd,tl);
  }
  rpmfiFree(fi);
  CAMLreturn(list_variant_L(tl));
}

#define fd_val(v) ((FD_t)(Field((v), 0)))

value rpm_parse_paragraph (value fd) {
  const char *s;
  CAMLparam1 ( fd );
  CAMLlocal2 ( hd, tl );
  CAMLlocal2 ( k, v);
  
  FD_t _fd = fd_val(fd);
  Header h;

  if ((h = headerRead(_fd, HEADER_MAGIC_YES)) == NULL) raise_Eof();

  tl = Val_emptylist;

  k = caml_copy_string("Package");
  s = headerGetAsString(h,RPMTAG_NAME);
  v = string_variant(caml_copy_string(s));
  hd = tuple(k,v);
  tl = append(hd,tl);

  k = caml_copy_string("Version");
  s = headerGetEVR(h, NULL);
  v = string_variant(caml_copy_string(s));
  hd = tuple(k,v);
  tl = append(hd,tl);

  k = caml_copy_string("Architecture");
  s = headerGetAsString(h,RPMTAG_ARCH);
  v = string_variant(caml_copy_string(s));
  hd = tuple(k,v);
  tl = append(hd,tl);

  k = caml_copy_string("Depends");
  v = get_deps(h,RPMTAG_REQUIRENAME);
  hd = tuple(k,v);
  tl = append(hd,tl);

  k = caml_copy_string("Provides");
  v = get_deps(h,RPMTAG_PROVIDENAME);
  hd = tuple(k,v);
  tl = append(hd,tl);

  k = caml_copy_string("Conflicts");
  v = get_deps(h,RPMTAG_CONFLICTNAME);
  hd = tuple(k,v);
  tl = append(hd,tl);

  k = caml_copy_string("Files");
  v = get_filedeps(h);
  hd = tuple(k,v);
  tl = append(hd,tl);

  if (h != NULL) (void) headerFree (h);

  CAMLreturn(tl);
}

value rpm_open_hdlist (value file_name) {
  CAMLparam1 (file_name);
  CAMLlocal1 (result);
  FD_t fd;

  fd = Fopen (String_val (file_name), "r");
  if (!fd) caml_failwith (strerror (errno));

  result = alloc_small(1, Abstract_tag);
  Field(result, 0) = (value) fd;

  CAMLreturn(result);
}

value rpm_close_hdlist (value fd) {
  CAMLparam1 (fd);
  Fclose (fd_val(fd));
  CAMLreturn(Val_unit);
}

value rpm_vercmp ( value x, value y ) {
  CAMLparam2 ( x , y );
  CAMLlocal1 ( res );
  res = rpmvercmp ( (char *) x , (char *) y );
  CAMLreturn (Val_int(res));
}

