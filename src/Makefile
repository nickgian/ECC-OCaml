LIBS =
PACKAGES = zarith
SOURCES = Ecc.ml register_dh.ml exchange_dh.ml sign.ml verify.ml
EXEC = register_dh exchange_dh sign verify
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLF = ocamlfind

SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)

all: $(OPTOBJS)
	$(CAMLF) $(CAMLOPT) -o register_dh $(LIBS:.cma=.cmxa) -package $(PACKAGES) -linkpkg Ecc.cmx register_dh.cmx
	$(CAMLF) $(CAMLOPT) -o exchange_dh $(LIBS:.cma=.cmxa) -package $(PACKAGES) -linkpkg Ecc.cmx exchange_dh.cmx
	$(CAMLF) $(CAMLOPT) -o sign $(LIBS:.cma=.cmxa) -package $(PACKAGES) -linkpkg Ecc.cmx sign.cmx
	$(CAMLF) $(CAMLOPT) -o verify $(LIBS:.cma=.cmxa) -package $(PACKAGES) -linkpkg Ecc.cmx verify.cmx

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly 

.ml.cmo:
	$(CAMLF) $(CAMLC) -c $(LIBS) -package $(PACKAGES) -linkpkg $<

.mli.cmi:
	$(CAMLF) $(CAMLC) -c $(LIBS) -package $(PACKAGES) -linkpkg $<

.ml.cmx:
	$(CAMLF) $(CAMLOPT) -c $(LIBS) -package $(PACKAGES) -linkpkg $<

clean::
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(EXEC)
	rm -f *.o
	rm -f $(EXEC).opt

.depend.input: Makefile
	@echo -n '--Checking Ocaml input files: '
	@(ls $(SMLIY) $(SMLIY:.ml=.mli) 2>/dev/null || true) \
	     >  .depend.new
	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
	    (echo 'unchanged'; rm -f .depend.new) || \
	    (echo 'changed'; mv .depend.new .depend.input)

depend: .depend

.depend:: $(SMLIY) .depend.input
	@echo '--Re-building dependencies'
	$(CAMLDEP) $(SMLIY) $(SMLIY:.ml=.mli) > .depend

include .depend
