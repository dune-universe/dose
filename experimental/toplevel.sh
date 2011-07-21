#!/bin/sh
d=`dirname $0`
d=`readlink -f "$d"`
if [ -f `which ledit` ]
then
	ledit ./pkglab.top -I $d/../_build/algo -I $d/../_build/common -I $d/../_build/libcudf -I $d/../_build/experimental -I $d/../_build/applications/boilerplates
else
	./pkglab.top -I $d/../_build/algo -I $d/../_build/common -I $d/../_build/libcudf -I $d/../_build/experimental -I $d/../_build/applications/boilerplates
fi	
