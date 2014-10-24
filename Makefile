#this is a forward reference to the target all below
all: all

#SHELL=/bin/bash
include Makefile.config

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

#VERBOSE := -classic-display
OBFLAGS := $(VERBOSE) -j 10 -no-links -cflags -warn-error,FPSXY
APPFLAGS := $(VERBOSE) -j 10
#OBFLAGS := $(OBFLAGS) -tag profile -tag debug
#OBFLAGS := $(OBFLAGS) -classic-display

all: itarget $(CAMLP4CMXS) $(BYTELIBS) $(OPTLIBS) $(CMXSLIBS) $(ALIBS) man
	$(OCAMLBUILD) $(APPFLAGS) applications/apps.otarget

apps: itarget $(CAMLP4CMXS) $(BYTELIBS) $(OPTLIBS) 
	$(OCAMLBUILD) $(APPFLAGS) applications/apps.otarget

cleandoselib:
	rm -Rf $(DOSELIBS)

itarget:
	@rm -f applications/apps.itarget
	@for i in $(TARGETS); do echo $$i >> applications/apps.itarget; done
	@$(shell \
		for lib in $(LIBNAMES); do \
			libname=`basename "$$lib"` ;\
			dirname=`dirname "$$lib"` ;\
			rm -f $$dirname/$$libname.itarget ;\
			for ext in $(SUFFIX); do \
				echo "$$libname.$$ext" >> $$dirname/$$libname.itarget; \
			done;\
		done)

_build/Camlp4MacroParser.cmxs:
	@mkdir -p _build
	ocamlopt -shared $(shell ocamlc -where)/camlp4/Camlp4Parsers/Camlp4MacroParser.cmx -o _build/Camlp4MacroParser.cmxs

$(DOSELIBS)/cudf.%:
	$(OCAMLBUILD) $(OBFLAGS) cudf/cudf.$*
	@mkdir -p $(DOSELIBS)
	@cp _build/cudf/*.cmi $(DOSELIBS)
	@for i in _build/cudf/cudf.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/common.%: common/*.ml
	$(OCAMLBUILD) $(OBFLAGS) common/common.otarget
	@mkdir -p $(DOSELIBS)
	@for i in _build/common/common.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/algo.%: algo/*.ml $(DOSELIBS)/common.%
	$(OCAMLBUILD) $(OBFLAGS) algo/algo.otarget
	@for i in _build/algo/algo.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/debian.%: deb/*.ml $(DOSELIBS)/algo.%
	$(OCAMLBUILD) $(OBFLAGS) deb/debian.otarget
	@for i in _build/deb/debian.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/rpm.%: rpm/*.ml $(DOSELIBS)/algo.%
	$(OCAMLBUILD) $(OBFLAGS) rpm/rpm.otarget
	@for i in _build/rpm/rpm.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/eclipse.%: eclipse/*.ml $(DOSELIBS)/debian.%
	$(OCAMLBUILD) $(OBFLAGS) eclipse/eclipse.otarget
	@for i in _build/eclipse/eclipse.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/csw.%: opencsw/*.ml $(DOSELIBS)/debian.%
	$(OCAMLBUILD) $(OBFLAGS) opencsw/csw.otarget
	@for i in _build/opencsw/csw.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/cv.%:
	$(OCAMLBUILD) $(OBFLAGS) cv/cv.otarget
	@for i in _build/cv/cv.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

$(DOSELIBS)/doseparse.%: $(DOSELIBS)/debian.% $(DOSELIBS)/eclipse.%
	$(OCAMLBUILD) $(OBFLAGS) doseparse/doseparse.otarget
	@for i in _build/doseparse/doseparse.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx $(DOSELIBS)/*.ml ; \
	  fi ; \
	done

$(DOSELIBS)/doseparseNoRpm.%: $(DOSELIBS)/debian.% $(DOSELIBS)/eclipse.%
	$(OCAMLBUILD) $(OBFLAGS) doseparseNoRpm/doseparseNoRpm.otarget
	@for i in _build/doseparseNoRpm/doseparseNoRpm.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
	  fi ; \
	done

clean:
	$(OCAMLBUILD) -clean
	rm -f applications/apps.itarget
	cd doc && $(MAKE) clean

distclean: clean
	rm -Rf Makefile.config aclocal.m4 config.log config.status autom4te.cache/
	rm -f algo/algo.mlpack
	rm -f common/versionInfo.ml
	rm -f db/db.mlpack
	rm -f _tags META

testapps: apps 
	@applications/dose-tests.py applications/dose-tests.list

testlib: 
	@for i in $(TESTS); do\
		echo "#######START TESTING $$i" ;\
		$(OCAMLBUILD) $(APPFLAGS) $$i/tests.$(OCAMLEXT) ;\
		./tests.$(OCAMLEXT) ;\
	done

test: testapps testlib

# stuff not not put in a distribution tarball
DIST_EXCLUDE = cudf tests $(wildcard */tests) experimental

INSTALL_STUFF_ = META
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.cma _build/doselibs/*.cmi)
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.cmxa _build/doselibs/*.cmxs)
INSTALL_STUFF_ += $(wildcard _build/doselibs/*.a)
INSTALL_STUFF_ += $(wildcard _build/*/*.mli)
INSTALL_STUFF_ += $(wildcard _build/rpm/*.so)

exclude_cudf = $(wildcard _build/doselibs/*cudf* _build/cudf/*)
INSTALL_STUFF = $(filter-out $(exclude_cudf), $(INSTALL_STUFF_))

install: META installcudf
	@test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	@test -d $(LIBDIR)/stublibs || mkdir -p $(LIBDIR)/stublibs
	@$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	@cd _build/applications ; \
	install -d $(BINDIR) ; \
	for f in $$(ls *.$(OCAMLEXT)) ; do \
	  install $(INSTALLOPTS) $$f $(BINDIR)/$${f%.$(OCAMLEXT)} ; \
	done
	@ln -s $(BINDIR)/distcheck $(BINDIR)/debcheck
	@ln -s $(BINDIR)/distcheck $(BINDIR)/rpmcheck
	@ln -s $(BINDIR)/distcheck $(BINDIR)/eclipsecheck
	@echo "Install dose librairies to $(LIBDIR)"
	@echo "Install dose binaries to $(BINDIR)"

uninstall: uninstallcudf
	@$(OCAMLFIND) remove -destdir $(LIBDIR) $(NAME)
	@for f in $$(ls *.$(OCAMLEXT)) ; do \
	  rm -f $(BINDIR)/$${f%.$(OCAMLEXT)} ; \
	done
	@rm -f $(BINDIR)/debcheck $(BINDIR)/rpmcheck $(BINDIR)/eclipsecheck
	@echo "Uninstall dose librairies from $(LIBDIR)"
	@echo "Uninstall dose binaries from $(BINDIR)"

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	@if [ -d ./$(DIST_DIR)/ ] ; then rm -rf ./$(DIST_DIR)/ ; fi
	@if [ -d ./$(DIST_TARBALL) ] ; then rm -f ./$(DIST_TARBALL) ; fi
	@if [ -d .svn ]; then \
	  svn export . ./$(DIST_DIR) ; \
	else \
	  mkdir ./$(DIST_DIR)/ ; git archive --format=tar HEAD | tar -x -C ./$(DIST_DIR)/ ; \
	fi
	@for f in $(DIST_EXCLUDE) ; do rm -rf ./$(DIST_DIR)/$$f; done
	@tar czf ./$(DIST_TARBALL) ./$(DIST_DIR)
	@rm -rf ./$(DIST_DIR)
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

changelog:
	dch -c CHANGES --package $(NAME) -v $(VERSION)

credits:
	@git log --pretty=format:'%aN        %aE' | LC_ALL=C sort -u | awk -F'\t' '{printf("\t%s <%s>\n",$$1,$$2)}';

doc: all
	$(OCAMLBUILD) dose3.docdir/index.html dose3.docdir/index.dot
	dot -Grotate=0 -Tsvg -o dose3.docdir/index.svg dose3.docdir/index.dot
	(cd doc && $(MAKE) all)

man:
	cd doc/manpages && $(MAKE)

upload: doc
	(cd doc && $(MAKE) upload)
	rsync -avz -O dose3.docdir/ scm.gforge.inria.fr:/home/groups/dose/htdocs/doc/api/

.PHONY: \
	common algo debian eclipse rpm cws doseparseNoRpm doseparse \
	all clean top-level headers test tags install uninstall dist doc man
