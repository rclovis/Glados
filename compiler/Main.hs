module Main (main) where

-- import Ast (genAst)
import Data.Maybe (fromJust)
-- import Eval (emptyEnv, exec, prelude)
import Lexer (tokenize)
-- import Sexpr (parseSexpr)
import System.Environment (getArgs)

import Data.Char (chr)

import Bytecode (IntTypes (..), getBin, bytecode, getIEEE, floatingStandardtoWord8, word8toChar)

import qualified Data.ByteString as B

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  contents <- readFile filename
  let tokens = tokenize contents
  let test = getBin bytecode
  B.putStr $ test