ML_SOURCES = lib.ml exp_3.ml

.PHONY: all
all: exp_3 lib.cmo dune

exp_3: $(ML_SOURCES)
	ocamlopt -g -o $@ $^

lib.cmo: lib.ml
	ocamlc -g -c -o $@ $^

.PHONY: run
run: exp_3
	./exp_3

.PHONY: dune
dune:
	dune build

.PHONY: clean
clean:
	rm -f exp_3 *.cm? *.o
	dune clean
