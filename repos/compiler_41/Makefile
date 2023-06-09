UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ARCH := elf64
endif
ifeq ($(UNAME), Darwin)
ARCH := macho64
endif

tests/%.s: tests/%.snek src/main.rs
	cargo run -- $< tests/$*.s

tests/%.run: tests/%.s runtime/start.rs
	nasm -f $(ARCH) tests/$*.s -o tests/$*.o
	ar rcs tests/lib$*.a tests/$*.o
	rustc -L tests/ -lour_code:$* runtime/start.rs -o tests/$*.run

%.run: %.s runtime/start.rs
	nasm -f $(ARCH) $*.s -o $*.o
	ar rcs lib$*.a $*.o
	rustc -L ./ -lour_code:$* runtime/start.rs -o $*.run

.PHONY: test
test:
	cargo build
	cargo test

clean:
	rm -f tests/*.a tests/*.s tests/*.run tests/*.o

submit:
	rm ./*.zip
	rm tests/*.run
	rm -r target/debug/*
	zip -r submit.zip ./*