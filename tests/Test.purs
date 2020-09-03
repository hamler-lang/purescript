module Test where

foreign import data IO :: Type -> Type
foreign import return :: [Integer] -> IO [Integer]
foreign import bind :: forall a b. IO a -> (a -> IO b) -> IO b

-- revFun :: [Integer] -> IO [Integer]
revFun ys = do
  -- v <- receive
  receive
         y -> revFun [y|ys]
       after 100 -> return ys
  -- return [10]
  -- after 100 -> (return ys :: IO [Integer])










