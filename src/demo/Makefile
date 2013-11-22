PACKAGES = ecc
ECDSADEMO = ecdsademo.ml
ECDHDEMO = ecdhdemo.ml
OCAMLC = ocamlc
OCAMLFIND = ocamlfind

default: ecdsa-demo ecdh-demo 

ecdsa-demo:
	$(OCAMLFIND) $(OCAMLC) -package $(PACKAGES) -linkpkg -o $@ $(ECDSADEMO)

ecdh-demo:
	$(OCAMLFIND) $(OCAMLC) -package $(PACKAGES) -linkpkg -o $@ $(ECDHDEMO)

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.a #*#
	rm -f ecdh-demo ecdsa-demo
