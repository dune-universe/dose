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

all:
	CPPFLAGS="$(CPPFLAGS)" LDFLAGS="-fstack-protector" $(OCAMLBUILD) $(OBFLAGS) $(TARGETS)

lib:
	CPPFLAGS="$(CPPFLAGS)" LDFLAGS="-fstack-protector" $(OCAMLBUILD) $(OBFLAGS) $(LIBS)

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
	rm common/_tags
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
INSTALL_STUFF += $(TARGETS)
INSTALL_STUFF += $(LIBS)

install:
	# install libraries
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	$(INSTALL) -patch-version $(VERSION) $(NAME) $(INSTALL_STUFF)

	# install applications
	test -d $(BINDIR) || mkdir -p $(BINDIR)
	cd _build/applications ; \
	for f in $$(ls *.$(OCAMLBEST)) ; do \
		cp $$f $(BINDIR)/$${f%.$(OCAMLBEST)}; \
	done

	@echo "Installed binaries into $(BINDIR)"

uninstall:
	$(UNINSTALL) $(NAME)
	if [ -f $(BINDIR)/XXXX ] ; then \
		rm $(BINDIR)/XXXXX ; \
	fi
	@echo "Removed $(BINDIR)/XXXX"

dist: ./$(DIST_TARBALL)
./$(DIST_TARBALL):
	if [ -d ./$(DIST_DIR)/ ] ; then rm -rf ./$(DIST_DIR)/ ; fi
	if [ -d ./$(DIST_TARBALL) ] ; then rm -f ./$(DIST_TARBALL) ; fi
	svn export . ./$(DIST_DIR)
	rm -rf ./$(DIST_DIR)/debian
	rm -rf ./$(DIST_DIR)/libcudf
	rm -rf ./$(DIST_DIR)/deb/tests
	rm -rf ./$(DIST_DIR)/rpm/tests
	rm -rf ./$(DIST_DIR)/common/tests
	rm -rf ./$(DIST_DIR)/algo/tests
	rm -rf ./$(DIST_DIR)/deb/libcudf
	rm -rf ./$(DIST_DIR)/rpm/libcudf
	rm -rf ./$(DIST_DIR)/eclipse/libcudf
	rm -rf ./$(DIST_DIR)/algo/libcudf
	rm -rf ./$(DIST_DIR)/common/libcudf
	rm -rf ./$(DIST_DIR)/applications/libcudf
	tar cvzf ./$(DIST_TARBALL) ./$(DIST_DIR)
	rm -rf ./$(DIST_DIR)
	@echo "Distribution tarball: ./$(DIST_TARBALL)"

doc:
	$(OCAMLBUILD) $(OBFLAGS) dose3.docdir/index.html

.PHONY: all opt clean top-level headers test tags install uninstall dist doc
