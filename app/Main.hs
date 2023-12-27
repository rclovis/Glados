module Main (main) where

import Ast (genAst)
import Data.Maybe (fromJust)
import Eval (emptyEnv, exec, prelude)
import Lexer (tokenize)
import Sexpr (parseSexpr)

main :: IO ()
main = do
  file <- readFile "testCode"
  let tokens = tokenize file
  -- print tokens
  print $ parseSexpr tokens
  -- let sex = parseSexpr tokens
  -- let ast = genAst sex
  -- exec prelude (fromJust ast)
