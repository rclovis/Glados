module Main (main) where

import Ast (genAst)
import Expr (genExpr)
import Lexer (tokenize)
import System.Environment (getArgs)

main :: IO ()
main = do
  fileNames <- getArgs
  file <- readFile (head fileNames)
  let ast = do
        tokens <- tokenize file
        genExpr tokens
  print ast
