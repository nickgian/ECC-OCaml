PACKAGES = zarith,sha
MLFILES = EccPrimitives.ml Ecdsa.ml Ecdh.ml
MLIFILES = EccPrimitives.mli Ecdsa.mli Ecdh.mli
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLFIND = ocamlfind
NAME = ecc

OBJS = $(MLFILES:.ml=.cmo)
CMIS = $(MLIFILES:.mli=.cmi)
CMAS = $(MLFILES:.ml=.cma)
OPTOBJS = $(MLFILES:.ml=.cmx)
INSTALL= META $(NAME).a $(NAME).cma $(NAME).cmxa $(CMIS) $(MLIS)

default: depend $(NAME) 

$(NAME): $(OBJS) $(OPTOBJS)
	$(OCAMLC) -a -o $@.cma $(OBJS)
	$(OCAMLOPT) -a -o $@.cmxa $(OPTOBJS)

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) -package $(PACKAGES) -c $< -o $@

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) -package $(PACKAGES) -c $< -o $@

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) -package $(PACKAGES) -c $< -o $@


clean:
	rm -f *.cm[ioxa] *~ .*~ *.o *.a #*#
	rm -f *.cmxa
	rm -rf _build
	dune clean

-include .depend

depend: $(MLFILES) $(MLIFILES)
	$(OCAMLDEP) $^ > .depend

install: $(INSTALL)
	$(OCAMLFIND) install $(NAME) $(INSTALL)

uninstall:
	$(OCAMLFIND) remove $(NAME)

run:
	make
	make uninstall
	make install
	cd test && make build
	cd test && make test
	make clean
	make uninstall
