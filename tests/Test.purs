module Test where

-- t x = let <<a,b,c>> = x in (a,b,c)
t x = let #{ 1 := a} = x  in a
t1 = #{1 => 2}