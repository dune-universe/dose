OCAMLBUILD=ocamlbuild
#ocamlbest
ifeq ("opt","opt")
  OCAMLBEST=native
  OCAMLLIBBEST=cmxa
else
  OCAMLBEST=byte
  OCAMLLIBBEST=cma
endif

%: %.ml
	$(OCAMLBUILD) $(OBFLAGS) $(<:.ml=.native)
	@touch _build/$@

clean:
	$(OCAMLBUILD) -clean
