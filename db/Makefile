NAME = nappe
VERSION = 0.1

LIBS = _build/db.cma
LIBS_OPT = _build/db.cmxa
RESULTS = $(LIBS)
RESULTS_OPT = $(LIBS_OPT)
SOURCES = $(wildcard *.ml *.mli)

OCAMLBUILD = ocamlbuild
# OBFLAGS = -classic-display
OBFLAGS =
OCAMLFIND = ocamlfind

DESTDIR =
LIBDIR = $(DESTDIR)/$(shell ocamlc -where)
BINDIR = $(DESTDIR)/usr/bin
COLLDIR = $(DESTDIR)/usr/lib/sqlite3
ifeq ($(DESTDIR),)
INSTALL = $(OCAMLFIND) install
UNINSTALL = $(OCAMLFIND) remove
else
INSTALL = $(OCAMLFIND) install -destdir $(LIBDIR)
UNINSTALL = $(OCAMLFIND) remove -destdir $(LIBDIR)
endif

all: $(RESULTS) $(RESULTS_OPT) sqlite
opt: $(RESULTS_OPT) sqlite
$(RESULTS): $(SOURCES)
$(RESULTS_OPT): $(SOURCES)

sqlite: 
	cd sqlite && make

clean:
	make -C sqlite clean
	$(OCAMLBUILD) $(OBFLAGS) -clean

_build/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

headers: header.txt .headache.conf
	headache -h header.txt -c .headache.conf $(SOURCES) $(C_LIB_SOURCES)

INSTALL_STUFF = META
INSTALL_STUFF += $(wildcard _build/*.cma _build/*.cmxa _build/*.a)
INSTALL_STUFF += $(wildcard *.mli)

INSTALL_STUFF += $(wildcard _build/db.o _build/db.cmx _build/db.cmi)

install:
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	test -d $(COLLDIR) || mkdir -p $(COLLDIR)
	if [ -f sqlite/libcollate_debian.so  ] ; then \
		cp sqlite/libcollate_debian.so  $(COLLDIR) ; \
	fi
	@echo "Installed $(COLLDIR)/libcollate_debian.so "

uninstall:
	$(UNINSTALL) $(NAME)
	if [ -f $(COLLDIR)/libcollate_debian.so  ] ; then \
		rm $(COLLDIR)/libcollate_debian.so  ; \
		rmdir $(COLLDIR) ; \
	fi
	@echo "Removed $(COLLDIR)/libcollate_debian.so "

.PHONY: all clean headers sqlite
