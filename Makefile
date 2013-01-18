LIBS = $(WITHUNIX)
PACKAGES = zarith
SOURCES = eccdh.ml
EXEC = eccdh
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
CAMLF = ocamlfind

SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(.cmx=.cmx)

#$(EXEC): $(OBJS)
#	$(CAMLC) $(CUSTOM) -o $(EXEC) $(LIBS) $(OBJS)

$(EXEC): $(OPTOBJS)
	$(CAMLF) $(CAMLOPT) -o $(EXEC) $(LIBS:.cma=.cmxa) -package $(PACKAGES) -linkpkg $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly 

#.ml.cmo:
#	$(CAMLF) $(CAMLC) -c $(LIBS) -package $(PACKAGES) -linkpkg $<

.mli.cmi:
	$(CAMLF) $(CAMLC) -c $(LIBS) -package $(PACKAGES) -linkpkg $<

.ml.cmx:
	$(CAMLF) $(CAMLOPT) -c $(LIBS) -package $(PACKAGES) -linkpkg $<

clean::
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(EXEC)
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
