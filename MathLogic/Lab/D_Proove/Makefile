all: compile

run:
	./run.sh
compile: 
	ghc -O2 -XBangPatterns -funbox-strict-fields Main.hs
gen:
	happy Parser.y
	alex Lexer.x
pack:	
	zip ../pack.zip -r * -x '.git*'
clean:
	rm -rf *.hi *.o Main Parser.hs Lexer.hs
.PHONY: run all gen pack clean compile
