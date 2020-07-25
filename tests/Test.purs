module Test where

foreign import data IO :: Type -> Type

foreign import tt :: IO String

data T = A Integer
       | B Integer
       | C Integer

t2 :: Integer -> Integer
t2 a | false = 0
     | true = 1

t4 :: String
t4  = receive
        A a -> "233"
        C 2 -> "great"
      after 2333 -> tt


