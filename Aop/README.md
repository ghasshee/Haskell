# Welcome to p2p Haskell code

We are learning "Algebra of Programming" written by Dr. Richard Bird.
The contents are about algebras which we mainly think with category theory or commutative diagrams.

# HowToUse

## Install Haskell

One major platform is [Haskell Platform](https://www.haskell.org/platform/).
Or, you can install [stack](https://docs.haskellstack.org/en/stable/README/).

## Download this Repository

you can just type on command line;

~~~
$ git clone http://www.github.com/p2p-education/aop
$ cd aop
~~~

## Start GHCi interpreter

start GHCi and load Codes.

~~~
$ ghci     //  or  $ stack ghci
λ> :l Tree.hs
λ> gt
+- 1
   +- 6
   |  +- 7
   +- 5
   |  +- 9
   |  |  +- 2
   |  |  +- 10
   |  +- 8
   +- 4
   +- 3
~~~


## Traversing the Tree Structure

~~~
$ ghci
λ> :l Inverse.hs
λ> wide gt
[10,2,8,9,7,3,4,5,6,1]
~~~
