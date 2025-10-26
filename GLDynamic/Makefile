ghc = ghc
src = eg.hs

all : 
	 $(ghc) $(src)
	 ghc ComplexFunAnime
	 ghc ComplexFun

test : all  
	./ComplexFunAnime
	./ComplexFun
	./eg

clean : 
	rm *.hi *.o eg ComplexFunAnime ComplexFun 
