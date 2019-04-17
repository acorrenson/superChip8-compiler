
all: parser clean

parser: parser.ml lexer.ml asembler.ml
	ocamlc -o asembler str.cma lexer.ml parser.ml asembler.ml

clean:
	rm -rf *.cmi *.cmo