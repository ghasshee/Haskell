all : asm 

asm : 
	ghc -o Asm Asm.hs -package deepseq -package hashtables -package parsec -package attoparsec -package hashmap 



clean : 
	rm -rf *.hi *.o ./Calc ./Main ./Asm 
