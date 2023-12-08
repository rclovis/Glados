module Main (main) where

import Lexer (Token (..), tokenize)

main :: IO ()
main = do
  file <- readFile "test.rkt"
  print $ tokenize file
