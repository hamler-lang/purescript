module Test where

foreign import error :: forall a.String -> a

data T a = T a | N

fun #{ 1 := (T x) } = x
fun #{ 1 := N } = N
fun _ = error "nice"