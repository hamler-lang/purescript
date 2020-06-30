module Test where

data T = T Integer | N

foreign import error :: forall a. String -> a

t1 :: (T,T) -> (Integer, Integer)
t1 (x,y) = let (T a, T b) = (x,y)
           in  (a,b)
