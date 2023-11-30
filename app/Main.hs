module Main (main) where

import Lib (someHello)
import Parser (parse)

main :: IO ()
main = parse "test.rkt"
