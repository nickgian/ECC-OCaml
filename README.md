ECC-OCaml
=========

Elliptic Curves Cryptography for OCaml

You need Zarith Library in order to compile:
http://forge.ocamlcore.org/projects/zarith

Compile with :
ocamlfind ocamlopt -o eccdh -package zarith -linkpkg eccdh.ml
