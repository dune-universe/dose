include Makefile.config

DIST_DIR = $(NAME)-$(VERSION)
DIST_TARBALL = $(DIST_DIR).tar.gz
DEBSRC = $(filter-out deb/myocamlbuild.ml deb/version.ml deb/format822.ml, $(wildcard deb/*.ml deb/*.mli))
DBSRC = $(filter-out db/myocamlbuild.ml, $(wildcard db/*.ml db/*.mli))
ALGOSRC = $(filter-out algo/myocamlbuild.ml algo/statistics.ml,$(wildcard algo/*.ml algo/*.mli))
APPSRC = $(filter-out applications/myocamlbuild.ml, $(wildcard applications/*.ml applications/*.mli))
RPMSRC = $(filter-out rpm/myocamlbuild.ml, $(wildcard rpm/*.ml rpm/*.mli rpm/*.h rpm/*.c))
COMSRC = $(filter-out common/myocamlbuild.ml common/edosSolver.ml common/edosSolver.mli common/util.ml,\
				 $(wildcard common/*.ml common/*.mli))
# stuff not not put in a distribution tarball
DIST_EXCLUDE = debian libcudf deb/tests rpm/tests common/tests algo/tests deb/libcudf rpm/libcudf \
	eclipse/libcudf algo/libcudf common/libcudf applications/libcudf applications/tests \
	experimental

all: lib
	$(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

lib:
	$(OCAMLBUILD) $(OBFLAGS) $(LIBS)

clean:
	$(OCAMLBUILD) $(OBFLAGS) -clean
	@cd deb ; $(OCAMLBUILD) $(OBFLAGS) -clean 
	@cd rpm ; $(OCAMLBUILD) $(OBFLAGS) -clean
	@cd eclipse ; $(OCAMLBUILD) $(OBFLAGS) -clean
	@cd db ; $(OCAMLBUILD) $(OBFLAGS) -clean
	@cd algo ; $(OCAMLBUILD) $(OBFLAGS) -clean
	@cd applications ; $(OCAMLBUILD) $(OBFLAGS) -clean
	@echo ""

distclean: clean
	rm -Rf Makefile.config aclocal.m4 config.log config.status autom4te.cache/
	rm _tags
	rm algo/_tags algo/algo.mlpack
	rm applications/_tags
	rm applications/boilerplates/_tags
	rm common/_tags common/versionInfo.ml
	rm db/_tags db/db.mlpack
	rm deb/_tags
	rm rpm/_tags
	rm eclipse/_tags
	rm META

_build/%:
	$(OCAMLBUILD) $(OBFLAGS) $*
	@touch $@

headers: header.txt .headache.conf
	headache -h header.txt -c .headache.conf $(ALGOSRC) $(DEBSRC) $(DBSRC) $(APPSRC) $(RPMSRC) $(COMSRC)

test:
	@for i in $(TESTS); do\
		cd $$i ;\
		echo "#######TESTING $$i" ;\
		$(OCAMLBUILD) $(OBFLAGS) tests.$(OCAMLBEST) ;\
		./tests.$(OCAMLBEST) ;\
		cd .. ;\
	done

INSTALL_STUFF = META

INSTALL_STUFF += $(wildcard _build/algo/algo.cm* _build/algo/algo.[oa]})
INSTALL_STUFF += $(wildcard _build/common/common.cm* _build/common/common.[oa])
INSTALL_STUFF += $(wildcard _build/deb/debian.cm* _build/deb/debian.[oa])
INSTALL_STUFF += $(wildcard _build/rpm/rpm.cm* _build/rpm/rpm.[oa])
INSTALL_STUFF += $(wildcard _build/eclispe/eclispe.cm* _build/eclispe/eclispe.[oa])

install:
	# install libraries
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)
	test -d $(LIBDIR)/stublibs || mkdir -p $(LIBDIR)/stublibs
	cp $(wildcard _build/rpm/dllrpm_stubs.so) $(LIBDIR)/stublibs/

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
