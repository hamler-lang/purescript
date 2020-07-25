module Test where


data T = A Integer
       | B Integer
       | C Integer

t2 :: Integer -> Integer
t2 a | false = 0
     | true = 1

t3 :: String
t3  = receive  t2, t2 of
        A a | false -> "ncie"
            | false -> "233"
        B a -> "ncie"
        2 ->   "great"

