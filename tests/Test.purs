module Test where

data Maybe a = Nothing | Just a

getA :: Binary -> Maybe (Integer, Binary, Binary)
getA << a:24/big-integer , b:4/binary-little , c:3/binary >> = Just (a,b,c)
getA _                                                       = Nothing


