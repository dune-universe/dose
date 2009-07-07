
%module sqlitext

%{
#define SWIG_FILE_WITH_INIT
#include "sqlitext.h"
%}

%typemap(in) Connection* {
  $1 = (Connection*) $input;
}

int enable_extension(Connection*, int);
