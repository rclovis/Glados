module Main (main) where
import System.Environment (getArgs)
import LexerVm (vmToken)
import EvalVm (mainTest)

main :: IO ()
main = do
  fileNames <- getArgs
  file <- readFile (head fileNames)
  let tokens = vmToken file
  case tokens of
    Nothing -> putStrLn "Nothing"
    Just tokens -> mainTest tokens
