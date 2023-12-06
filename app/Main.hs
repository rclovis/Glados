module Main (main) where

import Parser (parse)

main :: IO ()
main = parse "test.rkt"
