module Test where

t x = let <<"bab", res/binary>> = x -- <<"babab">>
    in res

