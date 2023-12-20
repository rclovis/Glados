module Main (main) where

import Ast (genAst)
import Data.Maybe (fromJust)
import Eval (emptyEnv, exec, prelude)
import Lexer (tokenize)
import Sexpr (parseSexpr)

main :: IO ()
main = do
  file <- readFile "factorial.csm"
  let tokens = tokenize file
  let sex = parseSexpr tokens
  let ast = genAst sex
  exec prelude (fromJust ast)
