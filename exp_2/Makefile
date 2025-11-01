.PHONY: all
all: exp_2

exp_2: exp_2.ml
	ocamlopt -o $@ $^

.PHONY: run
run: exp_2
	./exp_2

.PHONY: dune
dune:
	dune build

.PHONY: clean
clean:
	rm -f exp_2 exp_2.cm* exp_2.o
	dune clean
