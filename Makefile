# Picked from https://stackoverflow.com/questions/714100/os-detecting-makefile
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	BIN_FORMAT = elf64
	TARGET = x86_64-linux
endif
ifeq ($(UNAME_S),Darwin) # for mac
	BIN_FORMAT = macho64
	TARGET = x86_64-darwin
endif
export CFLAGS ?= -target $(TARGET) -z noexecstack -g -m64 -fPIE -pie

F =  # nothing by default
src = # nothing by default

.PHONY: test

init:
	dune build @check

test:
	dune exec execs/run_test.exe -- test '$(F)'

ctest:
	dune exec execs/run_test.exe -- test '$(F)' -c

compile: 
	dune exec execs/run_compile.exe $(src)

compile-run: $(subst .src,.run,$(src))
	./$<

interp: 
	dune exec execs/run_interp.exe $(src)

%.run: %.o rt/sys.c
	clang -o $@ $(CFLAGS) rt/sys.c $<

%.o: %.s
	nasm -f $(BIN_FORMAT) -o $@ $<

%.s: %.src 
	dune exec execs/run_compile.exe $< > $@

%.exe:
	dune build execs/$@

clean: clean-tests
	rm -Rf _build

clean-tests:
	rm -f bbctests/*.s bbctests/*.o bbctests/*.run bbctests/*.result bbctests/*~
	rm -rf bbctests/*dSYM

