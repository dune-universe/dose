include Makefile.config

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

OBFLAGS := -j 10 -classic-display
#OBFLAGS := $(OBFLAGS) -tag debug -tag profile
#OBFLAGS := $(OBFLAGS) -classic-display

realall: $(BYTELIBS) $(ALIBS) $(OPTLIBS) $(CMXSLIBS) man
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

fast: $(OPTLIBS)
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

apps:
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

DOSELIBS = _build/doselibs

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

opencws/cws.%:
	$(OCAMLBUILD) $(OBFLAGS) opencws/cws.$*
	@for i in _build/opencws/cws.*; do \
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

POD = $(wildcard doc/manpages/*.pod)
MAN = $(patsubst %.pod,%.1,$(POD))
HTML = $(patsubst %.pod,%.html,$(POD))

man: $(MAN)
html: $(HTML)
	mkdir -p dose3.docdir/manpages
	cp doc/manpages/*.html dose3.docdir/manpages

doc/manpages/%.1: doc/manpages/%.pod
	pod2man --section 1 --center="DOSE Tools" --release "$(NAME) $(VERSION)" doc/manpages/$*.pod > $@

doc/manpages/%.html: doc/manpages/%.pod
	pod2html doc/manpages/$*.pod > $@

clean:
	$(OCAMLBUILD) -clean
	@echo ""

distclean: clean
	rm -Rf Makefile.config aclocal.m4 config.log config.status autom4te.cache/
	rm -f algo/algo.mlpack
	rm -f common/versionInfo.ml
	rm -f db/db.mlpack
	rm -f _tags META
	rm -f doc/manpages/*.1

$(DOSELIBS)/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

test:
	@for i in $(TESTS); do\
		echo "#######TESTING $$i" ;\
		$(OCAMLBUILD) $(OBFLAGS) $$i/tests.$(OCAMLBEST) ;\
		./tests.$(OCAMLBEST) ;\
	done

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
	$(MAKE) man
	$(MAKE) html

.PHONY: all opt clean top-level headers test tags install uninstall dist doc
