module Main (main) where
import System.Environment (getArgs)
import LexerVm (vmToken)
import EvalVm (mainTest)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  args <- getArgs
  file <- BL.readFile (head args)
  let tokens = vmToken file
  case tokens of
    Nothing -> putStrLn "Nothing"
    Just tokenss -> mainTest tokenss args
