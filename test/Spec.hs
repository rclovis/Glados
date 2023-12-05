import Test.HUnit
import Parser (
    Parser(..)
    , runParser
    , parseChar
    , parseString
    , parseAnyChar
    , parseOr
    , parseAnd
    , parseAndWith
    , parseMany
    , parseSome
    )

parseCharTest :: Test
parseCharTest = TestCase $ do
  assertEqual "parseCharTest" (Just ('a', "bc")) (runParser (parseChar 'a') "abc")
  assertEqual "parseCharTest" Nothing (runParser (parseChar 'a') "fabc")
  assertEqual "parseCharTest" Nothing (runParser (parseChar 'a') "")

tests :: Test
tests =
  TestList
    [ TestLabel "ParseCharTest" parseCharTest
    ]

main :: IO ()
main = do
  putStrLn "Running tests..."
  _ <- runTestTT tests
  putStrLn "Done."
