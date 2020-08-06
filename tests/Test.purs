module Test where

t = (1,2)

k (x,y) = (y,x)

class Eq a where
 eq :: a -> a -> Boolean

instance (Eq a, Eq b) =>  Eq (a , b) where
 eq (a, b) (c, d) = eq a c

