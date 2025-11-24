.PHONY: all
all: exp_2 dune

exp_2: exp_2.ml
	ocamlopt -g -o $@ $^

.PHONY: run
run: exp_2
	./exp_2

.PHONY: dune
dune:
	dune build

.PHONY: test
test:
	@echo 'No tests are defined for this experiment'

.PHONY: clean
clean:
	rm -f exp_2 exp_2.cm* exp_2.o
	dune clean
