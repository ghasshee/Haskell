module Nat (..) where 

-- Natural Numbers
data N = Z | S N

-- show NaturalNumbers
ve :: Integer -> N 
ve 0 = Z
ve n = S (ve (n-1))
ev :: N -> Integer
ev Z = 0
ev (S n) = 1+ (ev n)

-- Fold Nat 
foldn (c,h) Z = c
foldn (c,h) (S n) = h (foldn (c,h) n)

-- Binary Operators
plus m = foldn (m, S) 
mult m = foldn (Z, plus m)
expn m = foldn (S Z, mult m)

-- Projection 
outl(m,n) = m
outr(m,n) = n

-- Factorial Function
f (m,n) = ( S m, mult n (S m) )
fact = outr . ( foldn((Z,S Z), f) )

-- Fibonacci Function
g (m,n) = (n, plus m n)
fib = outl . foldn ((Z,S Z), g)

-- Ackerman Function
h s = (foldn (S Z, s)) . S
ack = foldn ( S, h )



