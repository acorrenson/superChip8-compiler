
all: parser clean

parser: parser.ml lexer.ml
	ocamlc -o parser str.cma lexer.ml parser.ml

clean:
	rm -rf *.cmi *.cmo