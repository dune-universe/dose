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

#include <rpm/rpmtypes.h>
#include <rpm/rpmlib.h>
#include <rpm/header.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#define Val_none Val_int(0)

static inline value Val_some( value v )
{   
  CAMLparam1( v );
  CAMLlocal1( some );
  some = caml_alloc(1, 0);
  Store_field( some, 0, v );
  CAMLreturn(some) ;
}

static inline value tuple( value a, value b) {
  CAMLparam2( a, b );
  CAMLlocal1( tuple );

  tuple = caml_alloc(2, 0);

  Store_field( tuple, 0, a );
  Store_field( tuple, 1, b );

  CAMLreturn(tuple);
}

static inline value append( value hd, value tl ) {
  CAMLparam2( hd , tl );
  CAMLreturn(tuple( hd, tl ));
}

char* join_strings(char *strings[], char* sep, int count) {
  char *result = NULL;
  int   length = 0;
  int   n;

  if (count < 1) return NULL;
  if (strings == NULL) return NULL;

  for (length = 1, n = 0; n < count; n++)
    length += strlen (strings[n]);

  if (sep != NULL)
    length += (count - 1) * strlen (sep);

  result = (char*)malloc(length);
  result[0] = '\0';

  for (n = 0; n < count; n++) {
    strncat (result, strings[n], length);

    if (sep != NULL && n + 1 < count)
      strncat (result, sep, length);
  }

  return result;
}

char * tag_to_string (int32_t tag) {
  char *res = "";
  switch (tag) {
    case RPMTAG_NAME:
      res = "package";
      break;
    case RPMTAG_VERSION:
      res = "version";
      break;
    case RPMTAG_RELEASE:
      res = "release";
      break;
    case RPMTAG_EPOCH:
      res = "epoch";
      break;

    case RPMTAG_REQUIRENAME:
      res = "requirename";
      break;
    case RPMTAG_REQUIREVERSION:
      res = "requireversion";
      break;
    case RPMTAG_REQUIREFLAGS:
      res = "requireflags";
      break;

    case RPMTAG_PROVIDENAME:
      res = "providename";
      break;
    case RPMTAG_PROVIDEVERSION:
      res = "provideversion";
      break;
    case RPMTAG_PROVIDEFLAGS:
      res = "provideflags";
      break;

    case RPMTAG_CONFLICTNAME:
      res = "conflictname";
      break;
    case RPMTAG_CONFLICTVERSION:
      res = "conflictversion";
      break;
    case RPMTAG_CONFLICTFLAGS:
      res = "conflictflags";
      break;

    case RPMTAG_OBSOLETENAME:
      res = "obsoletename";
      break;
    case RPMTAG_OBSOLETEVERSION:
      res = "obsoleteversion";
      break;
    case RPMTAG_OBSOLETEFLAGS:
      res = "obsoleteflags";
      break;

    case RPMTAG_ARCH:
      res = "arch";
      break;

    case RPMTAG_DIRINDEXES:
      res = "dirindexes";
      break;
    case RPMTAG_BASENAMES:
      res = "basenames";
      break;
    case RPMTAG_DIRNAMES:
      res = "dirnames";
      break;


    case RPMTAG_FILEMODES:
      res = "filemodes";
      break;
    case RPMTAG_FILEMD5S:
      res = "filemd5s";
      break;
    case RPMTAG_RPMVERSION:
      res = "rpmversion";
      break;

    case RPMTAG_ARCHIVESIZE:
      res = "asize";
      break;
    case RPMTAG_SIZE:
      res = "size";
      break;
    case RPMTAG_SOURCERPM:
      res = "source";
      break;
    default:
      asprintf (&res, "%d", tag);
      break;
  }
  return res;
}

value assoc ( char* str, int32_t tag, int32_t type, rpm_constdata_t data, int32_t count) {
  CAMLparam0 ();
  CAMLlocal2( a, b );
	char *tmp = NULL;
  char **stra;
  int i;

  switch (type) {
    case RPM_STRING_TYPE:
      tmp = strdup((char *) data);
      break;
    case RPM_INT16_TYPE:
#if RPM_FORMAT_VERSION >= 5
    case RPM_UINT16_TYPE:
#endif
      stra = (char **) malloc (count * sizeof (char *));
      for (i = 0; i < count; i++) {
        asprintf (&stra[i], "%u", (((uint16_t *) data) [i]));
      }
      tmp = join_strings (stra, ",", count);
			for(i = 0 ; i<count ; i++)
				free(stra[i]);
      break;
    case RPM_INT32_TYPE:
      stra = (char **) malloc (count * sizeof (char *));
      for (i = 0; i < count; i++) {
        asprintf (&stra[i], "%d", (((int32_t *) data) [i]));
      }
      tmp = join_strings (stra, ",", count);
			for(i = 0 ; i<count ; i++)
				free(stra[i]);
      break;
    case RPM_I18NSTRING_TYPE:
    case RPM_STRING_ARRAY_TYPE:
      stra = (char **) malloc (count * sizeof (char *));
      for (i = 0; i < count; i++) {
        stra[i] = strdup(((char **) data) [i]);
      }
      tmp = join_strings (stra, ",", count);
			for(i = 0 ; i<count ; i++)
				free(stra[i]);
      break;
		default:
      tmp = NULL;
      break;
  }

  a = caml_copy_string(str);
  if (tmp != NULL) {
      b = caml_copy_string(tmp);
      free(tmp);
  } 
  else
      b = caml_copy_string("");

  CAMLreturn(tuple(a,b));
}

#define fd_val(v) ((FD_t)(Field((v), 0)))

value rpm_parse_paragraph (value fd) {
  CAMLparam1 ( fd );
  CAMLlocal2 ( hd, tl );

  Header header;
  HeaderIterator iter;
  struct rpmtd_s td;

  FD_t _fd;

  tl = Val_emptylist;
  _fd = fd_val(fd);

  header = headerRead(_fd, HEADER_MAGIC_YES);
  if (header == NULL) 
    CAMLreturn(Val_none);

  iter = headerInitIterator(header);
  while (headerNext(iter, &td)) {
    // we consider only meaninful tags. We ignore everything else
    // otherwise parsing and copy strings around takes forever
    switch (td.tag) {
      case RPMTAG_NAME:
      case RPMTAG_VERSION:
      case RPMTAG_RELEASE:
      case RPMTAG_EPOCH:
      case RPMTAG_REQUIRENAME:
      case RPMTAG_REQUIREVERSION:
      case RPMTAG_REQUIREFLAGS:
      case RPMTAG_PROVIDENAME:
      case RPMTAG_PROVIDEVERSION:
      case RPMTAG_PROVIDEFLAGS:
      case RPMTAG_CONFLICTNAME:
      case RPMTAG_CONFLICTVERSION:
      case RPMTAG_CONFLICTFLAGS:
      case RPMTAG_OBSOLETENAME:
      case RPMTAG_OBSOLETEVERSION:
      case RPMTAG_OBSOLETEFLAGS:
      case RPMTAG_ARCH:
      case RPMTAG_ARCHIVESIZE:
      case RPMTAG_SIZE:
      case RPMTAG_BASENAMES:
      case RPMTAG_DIRINDEXES:
      case RPMTAG_DIRNAMES:
      case RPMTAG_FILEMODES:
        hd = assoc(tag_to_string(td.tag),td.tag,td.type,td.data,td.count);
        tl = append(hd,tl);
        break;
      default:
        break;
    }
    rpmtdFreeData(&td);
  }
  if (iter != NULL) headerFreeIterator(iter);
  if (header != NULL) headerFree (header);
  CAMLreturn(Val_some(tl));
}

value rpm_parse_hdlists (value fd) {
  CAMLparam1 ( fd );
  CAMLlocal3 ( hd, tl, tll);

  Header header;
  HeaderIterator iter;
  struct rpmtd_s td;

  FD_t _fd;

  _fd = fd_val(fd);
  tll = Val_emptylist;

  header = headerRead(_fd, HEADER_MAGIC_YES);
  if (header == NULL) 
    CAMLreturn(tll);

  while ((iter = headerInitIterator(header)) != NULL) {
    tl = Val_emptylist;
    while (headerNext(iter, &td)) {
      hd = assoc(tag_to_string(td.tag),td.tag,td.type,td.data,td.count);
      tl = append(hd,tl);
      rpmtdFreeData(&td);
    }
    headerFreeIterator(iter);
    tll = append(tl,tll);
  }

  if (header != NULL) headerFree (header);
  CAMLreturn(tll);
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

/*
value rpm_EVRcmp ( value x, value y ) {
  CAMLparam2 ( x , y );
  CAMLlocal1 ( res );
  res = rpmEVRcmp ( (char *) x , (char *) y );
  CAMLreturn (Val_int(res));
}
*/
