SUBDIRS = manpages debcheck.primer apt-external-solvers.primer
#SHELL=/bin/bash

all:
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE)); \
	done

clean:
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE) clean); \
	done

webpages:
	cd webpages && $(MAKE) rebuild

upload: webpages
	rsync -avz -O --no-perms manpages/*.html scm.gforge.inria.fr:/home/groups/dose/htdocs/doc/man/
	rsync -avz -O --no-perms debcheck.primer/*.{html,pdf} scm.gforge.inria.fr:/home/groups/dose/htdocs/doc/tools/
	rsync -avz -O --no-perms webpages/public_html/* scm.gforge.inria.fr:/home/groups/dose/htdocs/public_html
