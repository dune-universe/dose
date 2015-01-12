all: rebuild

docdir:
	for i in ../../dose3.docdir/*.html ; \
		do ./extract-ocamldoc.py $$i > src/API/`basename $$i`; \
	done

refresh:
	ikiwiki --setup local.setup --verbose --refresh

rebuild:
	ikiwiki --setup local.setup --verbose --rebuild

clean:
	rm -Rf public_html

.phony: refresh rebuild clean
