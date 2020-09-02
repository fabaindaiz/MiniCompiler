F =  # nothing by default

.PHONY: test

init:
	dune build @check

test:
	dune exec src/execs/test.exe -- test '$(F)'

ctest:
	dune exec src/execs/test.exe -- test '$(F)' -c

%.exe:
	dune build src/execs/$@

clean: clean-tests
	rm -Rf _build

clean-tests:
	rm -f src/tests/*.s \
				src/tests/*.o \
				src/tests/*.run \
				src/tests/*.result \
				src/tests/*~

