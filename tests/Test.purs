module Test where

infixr 4 greatThan as >
infixr 4 lessThan  as <

foreign import greatThan :: Integer -> Integer -> Boolean
foreign import lessThan :: Integer -> Integer -> Boolean


f :: Integer -> String
f n | n > 0 = "Positive Integer"
    | n < 0 = "Negative Integer"
    | otherwise = "Zero"