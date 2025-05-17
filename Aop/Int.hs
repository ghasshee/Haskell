module Aop.Int where 

foldn h c 0 = c
foldn h c n = h (foldn h c (n-1))

plus m = foldn  succ    m
mult m = foldn (plus m) 0 
expn m = foldn (mult m) 1

outl(m,n) = m
outr(m,n) = n

fact = outr.foldn f (0,1) 
    where f(m,n) = (m+1, (m+1)*n)

fib = outl . foldn g (0,1)
    where g (m,n) = (n, m+n)

h s = foldn s 1 . succ
ack = foldn h succ

zero x  = 0
one  x  = 1

