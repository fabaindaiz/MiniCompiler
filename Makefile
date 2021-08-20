F =  # nothing by default

.PHONY: test

init:
	dune build @check

test:
	dune exec execs/test.exe -- test '$(F)'

ctest:
	dune exec execs/test.exe -- test '$(F)' -c

%.exe:
	dune build execs/$@

clean:
	rm -Rf _build



