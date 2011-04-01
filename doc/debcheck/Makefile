MAIN=debcheck
SHELL=/bin/bash
DEPS = $(wildcard *.tex)

$(MAIN).html: $(DEPS) $(MAIN).aux
	bibtex $(MAIN)	
	hevea -fix $(MAIN).tex

$(MAIN).ps: $(MAIN).dvi
	dvips $<

$(MAIN).dvi: $(DEPS)
	latex $(MAIN)

$(MAIN).aux: $(DEPS)
	latex $(MAIN)

headers:
	headache -h headache.header -c headache.conf $(DEPS)

clean:
	-rm -f $(MAIN).{dvi,aux,toc,haux,htoc,log,bbl,blg}

veryclean: clean
	-rm -f $(MAIN).{html,ps}	

.phony: clean realclean headers
