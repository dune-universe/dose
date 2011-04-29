# FreeBSD
CFLAGS=-I/usr/local/include
LDFLAGS=-L/usr/local/lib
# CFLAGS=""
# LDFLAGS=""

all:
	swig -python sqlitext.i
	CFLAGS=$(CFLAGS) LDFLAGS=$(LDFLAGS) python setup.py build_ext --inplace

clean:
	python setup.py clean -a
	rm -f _sqlitext.so sqlitext.py sqlitext_wrap.c sqlitext.pyc

