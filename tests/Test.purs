module Test where

t2 = do
   return 1 
   fun1
   receive
        1 -> return 12
        2 -> return 23


-- t3 = receive
--         1 -> return 12
--         2 -> return 23
--      after 1000 -> nn