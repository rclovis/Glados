import Lib (someFunc)
import Test.HUnit

someFuncTest :: Test
someFuncTest = TestCase (assertEqual "func" someFunc 1)

addTest :: Test
addTest = TestCase (assertEqual "1 + 1 = 2" (2 :: Integer) (1 + 1))

tests :: Test
tests =
  TestList
    [ TestLabel "AddTest" addTest,
      TestLabel "SomeFuncTest" someFuncTest
    ]

main :: IO ()
main = do
  putStrLn "Running tests..."
  _ <- runTestTT tests
  putStrLn "Done."
