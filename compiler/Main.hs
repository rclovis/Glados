module Main (main) where

import Ast.Ast (genAst)
import Ast.Expr (genExpr)
import Lexer (tokenize)
import System.Environment (getArgs)

import Data.Char (chr)

import Bytecode (getBin, bytecode, getHumanReadable)
import BuildBytecode (mainBytecodeTest)

import qualified Data.ByteString as B

main :: IO ()
main = do
  fileNames <- getArgs
  file <- readFile (head fileNames)
  let ast = do
        tokens <- tokenize file
        expr <- genExpr tokens
        genAst expr
  print ast
