module Main (main) where

import Ast.Ast (genAst)
import Ast.Expr (genExpr)
import Lexer (tokenize)
import System.Environment (getArgs)

import Bytecode (getBin, getHumanReadable)
import BuildBytecode (astToBytecode)
import Preprocessing (preprocessing)

import qualified Data.ByteString as B

main :: IO ()
main = do
  fileNames <- getArgs
  file <- preprocessing fileNames
  putStr file
  let ast = do
        tokens <- tokenize file
        expr <- genExpr tokens
        genAst expr
  putStrLn "AST--------------------------------------"
  putStrLn (show ast)
  putStrLn "-----------------------------------------"
  case ast of
    Nothing -> putStrLn "Error"
    Just ast' -> do
      let bc = astToBytecode ast'
      B.writeFile "out.bin" (getBin bc)
      putStrLn "BYTECODE---------------------------------"
      putStrLn (getHumanReadable bc)
      putStrLn "-----------------------------------------"
