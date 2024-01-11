import Test.HUnit
import TestAst (astTestSuite)
import TestParser (parserTestSuite)

main :: IO ()
main = do
  putStrLn "Running tests..."
  _ <- runTestTT parserTestSuite
  _ <- runTestTT astTestSuite
  putStrLn "Done."
