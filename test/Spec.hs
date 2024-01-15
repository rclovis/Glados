import FTestAst (astTestSuite)
import Test.HUnit
import UTestParser (parserTestSuite)

main :: IO ()
main = do
  putStrLn "Running tests..."
  _ <- runTestTT parserTestSuite
  _ <- runTestTT astTestSuite
  putStrLn "Done."
