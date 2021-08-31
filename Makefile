F =  # nothing by default

.PHONY: test

init:
	dune build @check

test:
	dune exec execs/test.exe -- test '$(F)'

ctest:
	dune exec execs/test.exe -- test '$(F)' -c

%.run: %.o rt/sys.c
	clang -o $@ $(CFLAGS) rt/sys.c $<

%.o: %.s
	nasm -f $(BIN_FORMAT) -o $@ $<

%.exe:
	dune build execs/$@

clean: clean-tests
	rm -Rf _build

clean-tests:
	rm -f tests/*.s \
				tests/*.o \
				tests/*.run \
				tests/*.result \
				tests/*~

