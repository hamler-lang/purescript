module Test where

t1 = {name="nice", age=23, pos= {x=10, y=20}}

t3 x = x

f x y = y

t2 = f 2 t3 t1{name="000", pos={x=41}}

