module Test where

data T = T | N | M | Q


t :: (T,T) -> T
t (T,_) = T
t (M,_) = N
t (x,N) = N
t (y,Q) = y
t (N,x) = N
t (Q,x) = N

j :: List T -> List T
j [] = []
j [T|y] = []
j [N|y] = []
j [M|y] = []
j [Q|y] = []
