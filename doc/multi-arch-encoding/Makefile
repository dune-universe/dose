NAME = main
DEPS = $(wildcard *.tex)
DEPS += build-pics-stamp
DEPS += build-tables-stamp
HEVEA=hevea -exec xxdate.exe

all: pdf
pdf: $(NAME).pdf
dvi: $(NAME).dvi
html: $(NAME).html
$(NAME).pdf $(NAME).dvi: $(DEPS)
show: showpdf
showpdf: $(NAME).pdf
	xdg-open $<
showdvi: $(NAME).dvi
	xdvi $<

$(NAME).html: $(DEPS) $(NAME).aux
	$(HEVEA) -fix $(NAME).tex

build-pics-stamp: $(wildcard pics/*.dia pics/*.dot)
	touch $@

build-tables-stamp: $(wildcard competition/*.tbl)
	touch $@
clean::
	-rm -f build-pics-stamp build-tables-stamp

# Debian package: latex-make
include /usr/include/LaTeX.mk

