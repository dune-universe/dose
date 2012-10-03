SUBDIRS = manpages debcheck.primer

all:
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE)); \
	done

clean:
	@for subdir in $(SUBDIRS); do \
	  (cd $$subdir && $(MAKE) clean); \
	done
