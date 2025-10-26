.PHONY: all
all: sample

sample: sample.o
	$(CC) -o $@ $^

sample.o: sample.s
	$(AS) -o $@ $^

sample.s: sample.i
	$(CC) -S -o $@ $^

sample.i: sample.c
	$(CPP) $(CPPFLAGS) -o $@ $^

.PHONY: preprocess
preprocess: sample.i

.PHONY: assemble
assemble: sample.s

.PHONY: compile
compile: sample.o

.PHONY: link
link: sample

.PHONY: run
run: link
	./sample

.PHONY: clean
clean:
	rm -f sample sample.o sample.s sample.i
