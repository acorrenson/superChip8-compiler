
all: assembler clean

assembler: parser.ml lexer.ml assembler.ml
	ocamlc -o assembler str.cma lexer.ml parser.ml assembler.ml

clean:
	rm -rf *.cmi *.cmo