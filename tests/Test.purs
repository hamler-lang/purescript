module Test where

data Maybe a = Nothing | Just a

getB :: Binary -> Maybe (Integer, Binary, Binary)
getB << a:4/big-integer , b:4/binary-little , c/binary >> = Just (a,b,c)
getB _                                                       = Nothing


