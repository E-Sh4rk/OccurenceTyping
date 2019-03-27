
all:
	ocamlbuild -use-ocamlfind main.native

test:
	./main.native

clean:
	ocamlbuild -clean
