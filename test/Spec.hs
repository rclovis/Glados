import Test.HUnit

addTest :: Test
addTest = TestCase (assertEqual "1 + 1 = 2" (2 :: Integer) (1 + 1))

tests :: Test
tests = TestList [TestLabel "AddTest" addTest]

main :: IO ()
main = do
  putStrLn "Running tests..."
  _ <- runTestTT tests
  putStrLn "Done."
