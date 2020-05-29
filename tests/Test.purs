module Test where

import Data.Map
import Data.Binary

-- foreign import error ::forall a.String -> a
-- foreign import showAny ::forall a.a -> String

div :: Integer -> Integer -> Integer
div  1 2 =1
div x y  =1


-- infixl 7 div as /

t1 :: Binary -> Integer
t1 <<a, (b):45 , (c):Integer-Litterl , (d):32:Littel-Binary>> = c
-- t1 <<a, (b):20:Integer-Little, c>> = b
t1 _ = 1


-- t2 :: Binary -> Integer
-- t2 x = let  <<(23):23/Big-Integer, (b):45/Littel-Integer , (c):32/Binary , (d):32/Binary>> = x
--        in  22
-- t2 _ = 1

-- g = t{nice =3}
-- t = { nice = 1 , greate = {nice = 10 , greate = 20}}


  -- | label '=' expr {% addFailure [$2] ErrRecordUpdateInCtr *> pure (RecordPun $ unexpectedName $ lblTok $1) }
  -- : binder {BinaryE () $1 Nothing Nothing }
  -- | '(' binder ')' ':'  int   {BinaryE () $2 $5 $7 }
  -- | '(' binder ')' ':'  int  ':' myPSString {BinaryE () $2 $5 $7 }


-- Ei = Value |
--      Value:Size |
--      Value/TypeSpecifierList |
--      Value:Size/TypeSpecifierList



