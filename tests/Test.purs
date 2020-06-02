module Test where

-- foreign import error :: forall a. String -> a
-- foreign import showAny :: forall a.  a -> String

-- t1 :: [Integer] -> [Integer]
-- t1 [a,b,c|t] = t
-- t1 _ = [1,2,3]

-- t2 x = let [a,b,c|t] = x
--        in (a,b,c,t)


infixr 6 t as :

t :: Integer -> Integer -> Integer
t a b = a

