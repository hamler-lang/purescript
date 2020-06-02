module Test where

infixr 6 t as :

t :: Integer -> Integer -> Integer
t a b = a


t1 = 1 : 2 : 3 : 4 : 5 : 6
-- infixr 6 t as : var t1 = t(1)(t(2)(t(3)(t(4)(t(5)(6)))));
-- infixl 6 t as : var t1 = t(t(t(t(t(1)(2))(3))(4))(5))(6);
