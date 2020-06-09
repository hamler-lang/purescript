module Test where

data A = A | B



fun :: [A] -> A
fun [] = A
fun [A] = A
fun [B] = A
fun [A,A] = A
fun [A,B] = A
fun [B,A] = A
fun [B,B] = A
fun [x,y,z|xs] = x



