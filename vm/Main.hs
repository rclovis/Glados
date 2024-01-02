module Main (main) where
import System.Environment (getArgs)
import EvalVm (vmToken)

main :: IO ()
main = do
  fileNames <- getArgs
  file <- readFile (head fileNames)
  let tokens = vmToken file
  print $ tokens
