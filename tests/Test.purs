module Test where


foreign import data Atom :: Type
foreign import atom :: String -> Atom
foreign import myfun :: Atom -> String

t = atom "nice"
t1 = :ncie
t2 = myfun t
t3 = myfun t1
t4 = myfun :nice1232
