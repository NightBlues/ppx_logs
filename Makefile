.PHONY: build clean test

build:
	dune build @all

clean:
	dune clean

test:
	dune runtest
