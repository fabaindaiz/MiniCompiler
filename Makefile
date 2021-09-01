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

interp: 
	dune exec execs/run_interp.exe $(src)

%.run: %.o rt/sys.c
	clang -o $@ $(CFLAGS) rt/sys.c $<

%.o: %.s
	nasm -f $(BIN_FORMAT) -o $@ $<

%.exe:
	dune build execs/$@

clean: clean-tests
	rm -Rf _build

clean-tests:
	rm -f bbctests/*.s bbctests/*.o bbctests/*.run bbctests/*.result bbctests/*~
	rm -rf bbctests/*dSYM

