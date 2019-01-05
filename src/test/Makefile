default: test

build:
	dune build test.exe --profile release
test:
	dune runtest -f
clean:
	rm -f *.cm[iox] *~ .*~ *.o *.a #*#
	rm -rf _build
	dune clean
