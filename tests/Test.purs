module Test where

data T = T Integer

foreign import data IO :: Type -> Type

foreign import return :: forall a. a -> IO a


t = receive
      T x -> return x
      1   -> return 23
      2   -> return 24
    after 1000 -> return 3

t1 = receive
      T x -> return x
      1   -> return 23
      2   -> return 24

t2 = 100

