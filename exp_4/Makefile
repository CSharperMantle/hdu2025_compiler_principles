MAINARGS ?=

.PHONY: all
all: dune

.PHONY: dune
dune:
	dune build

.PHONY: run
run: dune
	dune exec -- exp_4 $(MAINARGS)

.PHONY: test
test: dune
	scripts/run_tests.sh tests/testlist.tsv

.PHONY: clean
clean:
	dune clean
	rm -f exp_4 *.cm[iox] *.o *.cmti
	rm -f lexer.ml parser.ml parser.mli
	rm -f parser.conflicts parser.automaton
