SUBDIRS = manpages debcheck.primer apt-external-solvers.primer

all:
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE)); \
	done

clean:
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE) clean); \
	done

upload:
	rsync -avz manpages/*.html scm.gforge.inria.fr:/home/groups/dose/htdocs/doc/man/
