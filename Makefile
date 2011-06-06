include Makefile.config

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz

OBFLAGS = -use-ocamlfind #-classic-display

all: lib
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

DOSELIBS = _build/doselibs

common/common.%:
	$(OCAMLBUILD) $(OBFLAGS) common/common.$*
	@mkdir -p $(DOSELIBS)
	@for i in _build/common/common.{cmx,cmxa,cmxs,a,cmi,cma}; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  fi ; \
	done

algo/algo.%:
	$(OCAMLBUILD) $(OBFLAGS) algo/algo.$*
	@for i in _build/algo/algo.{cmx,cmxa,cmxs,a,cmi,cma}; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  fi ; \
	done

debian/debian.%:
	$(OCAMLBUILD) $(OBFLAGS) debian/debian.$*
	@for i in _build/debian/debian.{cmx,cmxa,cmxs,a,cmi,cma}; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  fi ; \
	done

rpm/rpm.%:
	$(OCAMLBUILD) $(OBFLAGS) rpm/rpm.$*
	@for i in _build/rpm/rpm.{cmx,cmxa,cmxs,a,cmi,cma}; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  fi ; \
	done

eclipse/eclipse.%:
	$(OCAMLBUILD) $(OBFLAGS) eclipse/eclipse.$*
	@for i in _build/eclipse/eclipse.{cmx,cmxa,cmxs,a,cmi,cma}; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  fi ; \
	done

doseparse/boilerplate.%:
	$(OCAMLBUILD) $(OBFLAGS) doseparse/boilerplate.$*
	@for i in _build/doseparse/boilerplate.{cmx,cmxa,cmxs,a,cmi,cma}; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  fi ; \
	done

doseparse/boilerplateNoRpm.%:
	$(OCAMLBUILD) $(OBFLAGS) doseparse/boilerplateNoRpm.$*
	@for i in _build/doseparse/boilerplateNoRpm.{cmx,cmxa,cmxs,a,cmi,cma}; do \
	  if [ -e $$i ]; then \
	  cp $$i $(DOSELIBS) ; \
	  fi ; \
	done

lib: $(LIBS)

clean:
	$(OCAMLBUILD) -clean

distclean: clean
	rm -Rf Makefile.config aclocal.m4 config.log config.status autom4te.cache/
	rm algo/algo.mlpack
	rm common/versionInfo.ml
	rm db/db.mlpack
	rm META

$(DOSELIBS)/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

test:
	@for i in $(TESTS); do\
		cd $$i ;\
		echo "#######TESTING $$i" ;\
		$(OCAMLBUILD) $(OBFLAGS) tests.$(OCAMLBEST) ;\
		./tests.$(OCAMLBEST) ;\
		cd .. ;\
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
	cp _build/rpm/dllrpm_stubs.so $(LIBDIR)/stublibs/

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

uninstall:
	rm -Rf $(LIBDIR)/dose3
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
