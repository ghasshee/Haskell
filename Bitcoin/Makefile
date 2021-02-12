all: 
	ghc Main.hs -L$$(nix eval nixpkgs.secp256k1.outPath | sed -e 's/\"//g' )/lib -lsecp256k1

clean: 
	rm *.hi *.o */*.hi */*.o ./Main 

