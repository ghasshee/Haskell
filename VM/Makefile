all: parse 
	ghc VMTranslate.hs 
	ghc Main.hs


lex: 
	alex Lexer.x
	ghc Lexer.hs 

parse: lex
	ghc Parser.hs 


test: all
	cat test/Basic/BasicTest.vm | ./Main
	#./Main test

clean: 
	rm -rf Main Lexer.hs *.o *.hi VMTranslate

