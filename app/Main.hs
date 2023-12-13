module Main (main) where

import Lexer (tokenize)
import Sexpr (parseSexpr)

main :: IO ()
main = do
  file <- readFile "test.rkt"
  let tokens = tokenize file
  print tokens
  let sex = parseSexpr tokens
  print sex
