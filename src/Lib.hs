module Lib
  ( someFunc,
    someHello,
  )
where

someHello :: IO ()
someHello = putStrLn "someFunc"

someFunc :: Int
someFunc = 1
