F =  # nothing by default

.PHONY: test

init:
	dune build @check

test:
	dune exec bin/test.exe -- test '$(F)'

ctest:
	dune exec bin/test.exe -- test '$(F)' -c

%.exe:
	dune build bin/$@

clean: clean-tests
	rm -Rf _build

clean-tests:
	rm -f tests/*.s \
				tests/*.o \
				tests/*.run \
				tests/*.result \
				tests/*~

