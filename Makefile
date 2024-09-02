# Picked from https://stackoverflow.com/questions/714100/os-detecting-makefile
ifeq ($(OS),Windows_NT) # for windows
	BIN_FORMAT = win64
	TARGET = x86_64-windows
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux) # for linux
		BIN_FORMAT = elf64
		TARGET = x86_64-linux
	endif
	ifeq ($(UNAME_S),Darwin) # for mac
		BIN_FORMAT = macho64
		TARGET = x86_64-darwin
	endif
endif
export CLANG_FLAGS ?= -target $(TARGET) -g -z noexecstack
export GCC_FLAGS ?= -g -z noexecstack

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
	clang -o $@ $(CLANG_FLAGS) rt/sys.c $<

%.o: %.s
	nasm -f $(BIN_FORMAT) -o $@ $<

%.s: %.src 
	dune exec execs/run_compile.exe $< > $@

%.exe:
	dune build execs/$@

clean: clean-tests
	rm -Rf _build

clean-tests:
	find bbctests -type f \( -name "*.s" -o -name "*.o" -o -name "*.run" -o -name "*.result" -o -name "*~" \) -exec rm -f {} +
	rm -rf bbctests/*dSYM

