MAIN=apt-external-solvers
SHELL=/bin/bash
HEVEA=hevea -exec xxdate.exe
DEPS = $(wildcard *.tex) version.tex mancoosi.bib

default: $(MAIN).html 

include ../../Makefile.config

$(MAIN).html: $(DEPS) $(MAIN).aux
	bibtex $(MAIN)	
	$(HEVEA) -fix $(MAIN).tex

$(MAIN).ps: $(MAIN).dvi
	dvips $<

$(MAIN).dvi: $(DEPS)
	latex $(MAIN)

$(MAIN).aux: $(DEPS)
	latex $(MAIN)

version.tex:
	echo -n "\\def\\version{" >> $@
	echo -n $(VERSION) >> $@
	echo "}" >> $@

headers:
	headache -h headache.header -c headache.conf $(DEPS)

clean:
	-rm -f $(MAIN).{dvi,aux,toc,haux,htoc,log,bbl,blg} version.tex

veryclean: clean
	-rm -f $(MAIN).{html,ps}	

.phony: clean realclean headers
