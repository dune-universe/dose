include Makefile.config

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz
DEB_TARBALL = $(subst -,_,$(DIST_DIR).orig.tar.gz)

all:
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

clean:
	$(OCAMLBUILD) $(OBFLAGS) -clean
	@for i in applications common deb rpm db; do\
		cd $$i ;\
		$(OCAMLBUILD) $(OBFLAGS) -clean\
	  cd .. ;\
	done

distclean: clean
	rm -Rf Makefile.config aclocal.m4 config.log config.status autom4te.cache/

_build/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

SOURCES = $(wildcard *.ml *.mli)
C_LIB_SOURCES = $(wildcard c-lib/*.c c-lib/*.h)

headers: header.txt .headache.conf
	headache -h header.txt -c .headache.conf $(SOURCES) $(C_LIB_SOURCES)

test: 
	@for i in $(TESTS); do\
		cd $$i ;\
		echo "==================TESTING $$i===================" ;\
		$(OCAMLBUILD) $(OBFLAGS) tests.$(OCAMLBEST) ;\
		./tests.$(OCAMLBEST) ;\
		cd .. ;\
	done

tags: TAGS

INSTALL_STUFF = META
INSTALL_STUFF += $(wildcard _build/*.cma _build/*.cmxa _build/cudf.a)
INSTALL_STUFF += $(wildcard _build/cudf_*.cmi) $(wildcard *.mli)
INSTALL_STUFF += $(wildcard _build/cudf_*.cmx _build/cudf_*.o _build/cudf_*.a)
INSTALL_STUFF += $(wildcard _build/cudf.o _build/cudf.cmx _build/cudf.cmi)

install:
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	test -d $(BINDIR) || mkdir -p $(BINDIR)
	if [ -f _build/cudf-check.native ] ; then \
		cp applications/_build/*.native $(BINDIR)/ ; \
	else \
		cp applications/_build/*.byte $(BINDIR)/ ; \
	fi
	@echo "Installed binaries in $(BINDIR)"

uninstall:
	$(UNINSTALL) $(NAME)
	if [ -f $(BINDIR)/cudf-check ] ; then \
		rm $(BINDIR)/cudf-check ; \
	fi
	@echo "Removed $(BINDIR)/cudf-check"

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	if [ -d ./$(DIST_DIR)/ ] ; then rm -rf ./$(DIST_DIR)/ ; fi
	if [ -d ./$(DIST_TARBALL) ] ; then rm -f ./$(DIST_TARBALL) ; fi
	svn export . ./$(DIST_DIR)
	rm -rf ./$(DIST_DIR)/debian
	tar cvzf ./$(DIST_TARBALL) ./$(DIST_DIR)
	rm -rf ./$(DIST_DIR)
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

./$(DEB_TARBALL): ./$(DIST_TARBALL)
	cp $< $@
deb: ./$(DEB_TARBALL)
	rm -rf ./$(DIST_DIR)
	tar xvzf $<
	svn export debian/ $(DIST_DIR)/debian
	cd $(DIST_DIR) && dpkg-buildpackage -rfakeroot

distcheck: ./$(DIST_TARBALL)
	tar xzf $<
	$(MAKE) -C ./$(DIST_DIR) all
	if which ocamlopt > /dev/null ; then $(MAKE) -C ./$(DIST_DIR) opt ; fi
	$(MAKE) -C ./$(DIST_DIR) test
	$(MAKE) -C ./$(DIST_DIR)/c-lib/ all
	$(MAKE) -C ./$(DIST_DIR) install DESTDIR=$(CURDIR)/$(DIST_DIR)/tmp
	rm -rf ./$(DIST_DIR)

doc:
	$(OCAMLBUILD) $(OBFLAGS) libmancoosi.docdir/index.html

.PHONY: all opt clean top-level headers test tags install uninstall dist doc
