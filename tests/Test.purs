module Test where

import Data.Map
import Data.Binary

-- foreign import error ::forall a.String -> a
-- foreign import showAny ::forall a.a -> String

div :: Integer -> Integer -> Integer
div  1 2 =1
div x y  =1


infixl 7 div as /

t1 :: Binary -> Integer
t1 <<(23):23:Big-Integer, (b):45:Littel-Integer , (c):32:Binary , (d):32:Binary>> = b
t1 _ = 1


-- t2 :: Binary -> Integer
-- t2 x = let  <<(23):23/Big-Integer, (b):45/Littel-Integer , (c):32/Binary , (d):32/Binary>> = x
--        in  22
-- t2 _ = 1


