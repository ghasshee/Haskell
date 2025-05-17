all : lexer parser lambda 
	ghc Parser.hs
lexer : 
	alex Lexer.x

lex : lexer 
	ghc test/LexerTest.hs
	test/LexerTest

parse : parser 
	ghc test/ParserTest.hs 
	test/ParserTest

parser : 
	happy Parser.y 

lambda : 
	ghc -o Lambda Lambda.hs 

clean : 
	rm -rf *.hi *.o Parser.hs Lexer.hs Lambda LexerTest ParserTest 
	rm -rf test/LexerTest
