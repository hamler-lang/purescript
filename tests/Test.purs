module Test where

t = do
  println "start"
  receive
     1 -> println "1"
     2 -> println "2"
  after 100 -> println "timeout"
  v <- receive
           1 -> return 1
           2 -> return 2
       after 100 -> return 3
  println $ "receive .. " showAny v

k = receive
      1 -> println "1"
      2 -> println "2"
    after 1000 -> println "timeout"


