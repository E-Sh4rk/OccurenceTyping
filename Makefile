
all:
	ocamlbuild -use-ocamlfind -use-menhir main.native

test:
	./main.native

clean:
	ocamlbuild -clean
