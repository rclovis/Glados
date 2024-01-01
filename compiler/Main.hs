module Main (main) where

-- import Ast (genAst)
import Data.Maybe (fromJust)
-- import Eval (emptyEnv, exec, prelude)
import Lexer (tokenize)
-- import Sexpr (parseSexpr)
import System.Environment (getArgs)

import Bytecode (toHexa, bytecode, getBinHexa, floatToHex)

main :: IO ()
main = do
  fileNames <- getArgs
  file <- readFile (head fileNames)
  let tokens = tokenize file
  -- print tokens
  print $ tokens
  -- let sex = parseSexpr tokens
  -- let ast = genAst sex
  -- exec prelude (fromJust ast)
  print $ getBinHexa bytecode 