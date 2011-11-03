include Makefile.config

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

OBFLAGS := -j 10 -use-ocamlfind
#OBFLAGS := $(OBFLAGS) -tag debug -tag profile
#OBFLAGS := $(OBFLAGS) -classic-display

all: $(BYTELIBS) $(ALIBS) $(OPTLIBS) $(CMXSLIBS) man
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

fast: $(OPTLIBS)
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

apps:
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

DOSELIBS = _build/doselibs

libcudf/cudf.%:
	$(OCAMLBUILD) $(OBFLAGS) libcudf/cudf.$*
	@mkdir -p $(DOSELIBS)
	@cp _build/libcudf/*.cmi $(DOSELIBS)
	@for i in _build/libcudf/cudf.*; do \
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

doseparse/boilerplate.%:
	$(OCAMLBUILD) $(OBFLAGS) doseparse/boilerplate.$*
	@for i in _build/doseparse/boilerplate.*; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  rm -f $(DOSELIBS)/*.mlpack $(DOSELIBS)/*.cmx ; \
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

man: doc/manpages/apt-cudf.1

doc/manpages/apt-cudf.1: doc/manpages/apt-cudf.pod
	pod2man --section 8 --release "$(NAME) $(VERSION)" $< > $@

clean:
	$(OCAMLBUILD) -clean

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
DIST_EXCLUDE = libcudf $(wildcard */tests) experimental


INSTALL_STUFF = META
INSTALL_STUFF += $(wildcard $(DOSELIBS)/*)

install:
	# install libraries
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	test -d $(LIBDIR)/stublibs || mkdir -p $(LIBDIR)/stublibs

	# eclipse and rpm to add ...
	for f in algo common deb ; do \
	  test -d  $(LIBDIR)/$(NAME)/$$f/ || mkdir -p  $(LIBDIR)/$(NAME)/$$f/ ; \
	  cp -f _build/$$f/*.mli $(LIBDIR)/$(NAME)/$$f/ ;\
	done

	# install applications
	test -d $(BINDIR) || mkdir -p $(BINDIR)
	cd _build/applications ; \
	for f in $$(ls *.$(OCAMLBEST)) ; do \
	  cp $$f $(BINDIR)/$${f%.$(OCAMLBEST)} ; \
	done

	if [ -e _build/rpm/dllrpm_stubs.so ]; then \
	  cp _build/rpm/dllrpm_stubs.so $(LIBDIR)/stublibs/ ;\
	fi

uninstall:
	rm -Rf $(LIBDIR)/$(NAME)
	rm $(LIBDIR)/stublibs/dllrpm_stubs.so

	for f in $$(ls *.$(OCAMLBEST)) ; do \
	  if [ -f $(BINDIR)/$${f%.$(OCAMLBEST)} ]; then \
	    rm $(BINDIR)/$${f%.$(OCAMLBEST)} ; \
	  fi \
	done

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

doc:
	$(OCAMLBUILD) $(OBFLAGS) dose3.docdir/index.html

.PHONY: all opt clean top-level headers test tags install uninstall dist doc
