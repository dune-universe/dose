#this is a forward reference to the target all below
all: all

include Makefile.config

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

OBFLAGS := -j 10 -classic-display
#OBFLAGS := $(OBFLAGS) -tag debug -tag profile
#OBFLAGS := $(OBFLAGS) -classic-display

all: $(CAMLP4CMXS) $(BYTELIBS) $(ALIBS) $(OPTLIBS) $(CMXSLIBS) man
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

fast: $(CAMLP4CMXS) $(OPTLIBS)
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

apps:
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

DOSELIBS = _build/doselibs

camlp4cmxs:
	mkdir -p _build
	ocamlopt -shared $(shell ocamlc -where)/camlp4/Camlp4Parsers/Camlp4MacroParser.cmx -o _build/Camlp4MacroParser.cmxs

cudf/cudf.%:
	$(OCAMLBUILD) $(OBFLAGS) cudf/cudf.$*
	@mkdir -p $(DOSELIBS)
	@cp _build/cudf/*.cmi $(DOSELIBS)
	@for i in _build/cudf/cudf.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

common/common.%:
	$(OCAMLBUILD) $(OBFLAGS) common/common.$*
	@mkdir -p $(DOSELIBS)
	@for i in _build/common/common.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

algo/algo.%:
	$(OCAMLBUILD) $(OBFLAGS) algo/algo.$*
	@for i in _build/algo/algo.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

deb/debian.%:
	$(OCAMLBUILD) $(OBFLAGS) deb/debian.$*
	@for i in _build/deb/debian.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

rpm/rpm.%:
	$(OCAMLBUILD) $(OBFLAGS) rpm/rpm.$*
	@for i in _build/rpm/rpm.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

eclipse/eclipse.%:
	$(OCAMLBUILD) $(OBFLAGS) eclipse/eclipse.$*
	@for i in _build/eclipse/eclipse.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

opencsw/csw.%:
	$(OCAMLBUILD) $(OBFLAGS) opencsw/csw.$*
	@for i in _build/opencsw/csw.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

doseparse/boilerplate.%:
	$(OCAMLBUILD) $(OBFLAGS) doseparse/boilerplate.$*
	@for i in _build/doseparse/boilerplate.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx $(DOSELIBS)/*.ml ; \
	  fi ; \
	done

doseparse/boilerplateNoRpm.%:
	$(OCAMLBUILD) $(OBFLAGS) doseparse/boilerplateNoRpm.$*
	@for i in _build/doseparse/boilerplateNoRpm.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

clean:
	$(OCAMLBUILD) -clean
	@echo ""
	cd doc && $(MAKE) clean

distclean: clean
	rm -Rf Makefile.config aclocal.m4 config.log config.status autom4te.cache/
	rm -f algo/algo.mlpack
	rm -f common/versionInfo.ml
	rm -f db/db.mlpack
	rm -f _tags META

$(DOSELIBS)/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

testapps: apps 
	@applications/dose-tests.py -pwd `pwd` -v

testlib: 
	@for i in $(TESTS); do\
		echo "#######TESTING $$i" ;\
		$(OCAMLBUILD) $(OBFLAGS) $$i/tests.$(OCAMLBEST) ;\
		./tests.$(OCAMLBEST) ;\
	done
	@make apps

test: testapps testlib

# stuff not not put in a distribution tarball
DIST_EXCLUDE = cudf tests $(wildcard */tests) experimental

INSTALL_STUFF_ = META
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.cma _build/doselibs/*.cmi)
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.cmxa _build/doselibs/*.cmxs)
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.a)
INSTALL_STUFF_ += $(wildcard _build/*/*.mli)
INSTALL_STUFF_ += $(wildcard _build/rpm/*.so _build/rpm/*.a)

exclude_cudf = $(wildcard _build/doselibs/*cudf* _build/cudf/*)
INSTALL_STUFF = $(filter-out $(exclude_cudf), $(INSTALL_STUFF_))

install: META installcudf
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	test -d $(LIBDIR)/stublibs || mkdir -p $(LIBDIR)/stublibs
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)

	# install applications
	cd _build/applications ; \
	install -d $(BINDIR) ; \
	for f in $$(ls *.$(OCAMLBEST)) ; do \
	  install $(INSTALLOPTS) $$f $(BINDIR)/$${f%.$(OCAMLBEST)} ; \
	done
	ln -s $(BINDIR)/distcheck $(BINDIR)/debcheck
	ln -s $(BINDIR)/distcheck $(BINDIR)/rpmcheck
	ln -s $(BINDIR)/distcheck $(BINDIR)/eclipsecheck

uninstall: uninstallcudf
	$(OCAMLFIND) remove -destdir $(LIBDIR) $(NAME)

	for f in $$(ls *.$(OCAMLBEST)) ; do \
	  rm -f $(BINDIR)/$${f%.$(OCAMLBEST)} ; \
	done
	rm -f $(BINDIR)/debcheck $(BINDIR)/rpmcheck $(BINDIR)/eclipsecheck

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	if [ -d ./$(DIST_DIR)/ ] ; then rm -rf ./$(DIST_DIR)/ ; fi
	if [ -d ./$(DIST_TARBALL) ] ; then rm -f ./$(DIST_TARBALL) ; fi
	if [ -d .svn ]; then \
	  svn export . ./$(DIST_DIR) ; \
	else \
	  mkdir ./$(DIST_DIR)/ ; git archive --format=tar HEAD | tar -x -C ./$(DIST_DIR)/ ; \
	fi
	for f in $(DIST_EXCLUDE) ; do rm -rf ./$(DIST_DIR)/$$f; done
	tar cvzf ./$(DIST_TARBALL) ./$(DIST_DIR)
	rm -rf ./$(DIST_DIR)
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

changelog:
	dch -c CHANGES --package $(NAME) -v $(VERSION)

credits:
	@git log --pretty=format:'%aN        %aE' | LC_ALL=C sort -u | awk -F'\t' '{printf("\t%s <%s>\n",$$1,$$2)}';

doc: fast
	$(OCAMLBUILD) $(OBFLAGS) dose3.docdir/index.html dose3.docdir/index.dot
	dot -Grotate=0 -Tsvg -o dose3.docdir/index.svg dose3.docdir/index.dot
	(cd doc && $(MAKE) all)

man:
	cd doc/manpages && $(MAKE)

upload: doc
	(cd doc && $(MAKE) upload)
	rsync -avz dose3.docdir/ scm.gforge.inria.fr:/home/groups/dose/htdocs/doc/api/

.PHONY: all opt clean top-level headers test tags install uninstall dist doc man
