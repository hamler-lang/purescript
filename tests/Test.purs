module Test where


t1 :: [Integer] -> [Integer]
t1 [a,b,c|t] = t
t1 _ = [1,2,3]
