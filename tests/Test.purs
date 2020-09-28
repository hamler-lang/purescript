module Test where

a = { name="yang"
    , pos={ x=10, y =20
          , info= { s ="aa"
                  , v=45.9
                  }
          }
    }


b = a{pos.info.s = "sdzx"}
