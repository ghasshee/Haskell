test: lex 
	./JackTokenizer Main.jack

lex: 
	ghc JackTokenizer.hs 

parse: lex
	ghc Parser.hs 



clean: 
	rm -rf Lexer.hs *.o *.hi Main JackTokenizer JackParser 

