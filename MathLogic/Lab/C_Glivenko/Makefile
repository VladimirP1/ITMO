all: compile

HOME=${PWD}
export HOME

test:
	echo ${HOME}
run:
	./Main
compile: 
	ghc -O2 -XBangPatterns -funbox-strict-fields Main.hs
gen:
	happy Parser.y
pack:	
	zip ../pack.zip -r * -x '.git*' #-x 'deps/*' -x '*.o' -x Main -x '.cabal*' -x '.ghc*' -x '*.hi' -x compile-deps
clean:
	rm -rf deps
	rm -rf *.hi *.o Main Parser.hs tmp .cabal .ghc compile-deps
.PHONY: run all gen pack clean compile
