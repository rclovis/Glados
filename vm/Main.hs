module Main (main) where
import System.Environment (getArgs)
import LexerVm (vmToken)
import EvalVm (mainTest)
import qualified Data.ByteString.Lazy as BL

import Data.Sequence as S


toSeq :: [a] -> S.Seq a
toSeq [] = S.empty
toSeq (x : xs) = x <| toSeq xs

main :: IO ()
main = do
  args <- getArgs
  file <- BL.readFile (head args)
  let tokens = vmToken file
  case tokens of
    Nothing -> putStrLn "Nothing"
    Just tokenss -> mainTest (toSeq tokenss) args
