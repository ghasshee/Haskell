all: 
	# ghc cube 
	ghc id 
	ghc z^{1-i}
	ghc z^sqrt_z
	ghc z_z2_z3
	ghc z^z
	ghc {z+2i}^3+{z-2i}^2
	ghc sqrt_z^z+1
	ghc fish
	ghc ComplexFun 
	# ghc Arrow 
	# ghc ComplexFunNew
	ghc Joukowski_Airfoil
	

test: all 
	# ./cube
	./id
	./z^{1-i}
	./z^sqrt_z
	./z_z2_z3
	./z^z
	./{z+2i}^3+{z-2i}^2
	./sqrt_z^z+1
	./fish 
	./ComplexFun 
	# ./Arrow
	# ./ComplexFunNew
	./Joukowski_Airfoil


clean : 
	rm id 
	rm fish 
	rm z\^\{1-i\} z_z2_z3 z\^z sqrt_z\^z+1 z\^sqrt_z
	rm \{z+2i\}\^3+\{z-2i\}\^2
	# rm cube Arrow ComplexFunNew
	rm ComplexFun Joukowski_Airfoil
	rm *.hi *.o
