MAIN=debcheck
SHELL=/bin/bash
DEPS = $(wildcard *.tex)

$(MAIN).html: $(DEPS)	
	hevea -fix $(MAIN).tex

$(MAIN).ps: $(MAIN).dvi
	dvips $<

$(MAIN).dvi: $(DEPS)
	latex $(MAIN)

clean:
	-rm -f $(MAIN).{dvi,aux,toc,haux,htoc,log}

veryclean: clean
	-rm -f $(MAIN).{html,ps}	

.phony: clean realclean
