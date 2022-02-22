
build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

tests:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f blackjack.zip
	zip -r blackjack.zip . -x@exclude.lst

clean:
	dune clean
	rm -f blackjack.zip

doc:
	dune build @doc
