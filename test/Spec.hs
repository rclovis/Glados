import Test.HUnit
import Parser (
    Parser(..)
    , parseChar
    , parseString
    , parseAnyChar
    , parseOr
    , parseAnd
    , parseAndWith
    , parseMany
    , parseSome
    , parseUInt
    , parseInt
    , runParser, parseList
    )

parseCharTest :: Test
parseCharTest = TestCase $ do
  assertEqual "parseCharTest" (Just ('a', "bc")) (runParser (parseChar 'a') "abc")
  assertEqual "parseCharTest" Nothing (runParser (parseChar 'a') "fabc")
  assertEqual "parseCharTest" Nothing (runParser (parseChar 'a') "")

parseStringTest :: Test
parseStringTest = TestCase $ do
  assertEqual "parseString" (Just ("abc", "")) (runParser (parseString "abc") "abc")
  assertEqual "parseString" (Just ("abc", "def")) (runParser (parseString "abc") "abcdef")
  assertEqual "parseString" Nothing (runParser (parseString "abc") "ab")
  assertEqual "parseString" Nothing (runParser (parseString "abc") "")

parseAnyCharTest :: Test
parseAnyCharTest = TestCase $ do
  assertEqual "parseAnyCharTest" (Just ('a', "bc")) (runParser (parseAnyChar "abc") "abc")
  assertEqual "parseAnyCharTest" (Just ('a', "bcdef")) (runParser (parseAnyChar "abc") "abcdef")
  assertEqual "parseAnyCharTest" Nothing (runParser (parseAnyChar "___f") "abcdef")
  assertEqual "parseAnyCharTest" Nothing (runParser (parseAnyChar "abc") "def")
  assertEqual "parseAnyCharTest" Nothing (runParser (parseAnyChar "abc") "")

parseOrTest :: Test
parseOrTest = TestCase $ do
  assertEqual "parseOrTest" (Just ('a', "bc")) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "abc")
  assertEqual "parseOrTest" (Just ('b', "cdef")) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcdef")
  assertEqual "parseOrTest" Nothing (runParser (parseOr (parseChar 'a') (parseChar 'b')) "cdef")
  assertEqual "parseOrTest" Nothing (runParser (parseOr (parseChar 'a') (parseChar 'b')) "")

parseAndTest :: Test
parseAndTest = TestCase $ do
  assertEqual "parseAndTest" (Just (('a', 'b'), "cdef")) (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcdef")
  assertEqual "parseAndTest" Nothing (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcdef")
  assertEqual "parseAndTest" Nothing (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "")

parseAndWithTest :: Test
parseAndWithTest = TestCase $ do
  assertEqual "parseAndWithTest" (Just ("ab", "cdef")) (runParser (parseAndWith (\a b -> [a, b]) (parseChar 'a') (parseChar 'b')) "abcdef")
  assertEqual "parseAndWithTest" Nothing (runParser (parseAndWith (\a b -> [a, b]) (parseChar 'a') (parseChar 'b')) "bcdef")
  assertEqual "parseAndWithTest" Nothing (runParser (parseAndWith (\a b -> [a, b]) (parseChar 'a') (parseChar 'b')) "")

parseManyTest :: Test
parseManyTest = TestCase $ do
  assertEqual "parseManyTest" (Just ("     ", "foobar")) (runParser (parseMany (parseChar ' ')) "     foobar")
  assertEqual "parseManyTest" (Just ("      ", "foobar")) (runParser (parseMany (parseChar ' ')) "      foobar")
  assertEqual "parseManyTest" (Just (" ", "foobar")) (runParser (parseMany (parseChar ' ')) " foobar")
  assertEqual "parseManyTest" (Just ("  ", "foobar")) (runParser (parseMany (parseChar ' ')) "  foobar")
  assertEqual "parseManyTest" (Just ("", "foobar    ")) (runParser (parseMany (parseChar ' ')) "foobar    ")
  assertEqual "parseManyTest" (Just ("  ", "")) (runParser (parseMany (parseChar ' ')) "  ")
  assertEqual "parseManyTest" (Just (" ", "")) (runParser (parseMany (parseChar ' ')) " ")

parseSomeTest :: Test
parseSomeTest = TestCase $ do
  assertEqual "parseSomeTest" (Just ("     ", "foobar")) (runParser (parseSome (parseChar ' ')) "     foobar")
  assertEqual "parseSomeTest" (Just ("      ", "foobar")) (runParser (parseSome (parseChar ' ')) "      foobar")
  assertEqual "parseSomeTest" (Just (" ", "foobar")) (runParser (parseSome (parseChar ' ')) " foobar")
  assertEqual "parseSomeTest" (Just ("  ", "foobar")) (runParser (parseSome (parseChar ' ')) "  foobar")
  assertEqual "parseSomeTest" Nothing (runParser (parseSome (parseChar ' ')) "foobar    ")
  assertEqual "parseSomeTest" (Just ("  ", "")) (runParser (parseSome (parseChar ' ')) "  ")
  assertEqual "parseSomeTest" (Just (" ", "")) (runParser (parseSome (parseChar ' ')) " ")

parseUIntTest :: Test
parseUIntTest = TestCase $ do
  assertEqual "parseUIntTest" (Just (123, "foobar")) (runParser parseUInt "123foobar")
  assertEqual "parseUIntTest" Nothing (runParser parseUInt "foobar")
  assertEqual "parseUIntTest" (Just (123, "foobar")) (runParser parseUInt "0123foobar")
  assertEqual "parseUIntTest" (Just (0, "u123foobar")) (runParser parseUInt "0u123foobar")
  assertEqual "parseUIntTest" Nothing (runParser parseUInt "d0u123foobar")

parseIntTest :: Test
parseIntTest = TestCase $ do
  assertEqual "parseIntTest" (Just (123, "foobar")) (runParser parseInt "123foobar")
  assertEqual "parseIntTest" (Just (-123, "foobar")) (runParser parseInt "-123foobar")
  assertEqual "parseIntTest" (Just (-12, "-3foobar")) (runParser parseInt "-12-3foobar")
  assertEqual "parseIntTest" (Just (12, "-3foobar")) (runParser parseInt "12-3foobar")
  assertEqual "parseIntTest" (Just (0, "foobar")) (runParser parseInt "-0foobar")
  assertEqual "parseIntTest" (Just (123, "foobar")) (runParser parseInt "+123foobar")
  assertEqual "parseIntTest" Nothing (runParser parseInt "foobar")
  assertEqual "parseIntTest" (Just (123, "foobar")) (runParser parseInt "0123foobar")
  assertEqual "parseIntTest" (Just (0, "u123foobar")) (runParser parseInt "0u123foobar")
  assertEqual "parseIntTest" Nothing (runParser parseInt "d0u123foobar")

parseListTest :: Test
parseListTest = TestCase $ do
  assertEqual "parseListTest" (Just ([1, 2, 3, 4, 5], "")) (runParser  (parseList(parseChar '(') (parseChar ')') (parseChar ' ') (parseChar ' ') parseInt) "(1 2 3 4 5)")
  assertEqual "parseListTest" (Just ([1, 2, 3, 4, 5], "")) (runParser  (parseList(parseChar '[') (parseChar ')') (parseChar ' ') (parseChar ' ') parseInt) "[ 1   2  3 4   5  )")
  assertEqual "parseListTest" (Just ([1, 2, 3, 4, 5], "")) (runParser  (parseList(parseChar '[') (parseChar ')') (parseChar ',') (parseChar ' ') parseInt) "[1,2,3,4,5)")
  assertEqual "parseListTest" (Just ([1, 2, 3, 4, 5], "")) (runParser  (parseList(parseChar '[') (parseChar ')') (parseChar ',') (parseChar ' ') parseInt) "[1 ,  2 , 3,   4,5)")
  assertEqual "parseListTest" (Just ([1, 2, 3, 4, 5], "")) (runParser  (parseList(parseChar '[') (parseChar ')') (parseChar ',') (parseChar ' ') parseInt) "[1 ,  2 , 3,   4,5)")
  assertEqual "parseListTest" (Just ([1, 2, 3, 4, 5], "yess")) (runParser  (parseList(parseChar '[') (parseChar ')') (parseChar ',') (parseAnyChar " \n\t") parseInt) "[1 , \n  2 ,3,4,5 )yess")
  assertEqual "parseListTest" (Just ([1], "yess")) (runParser  (parseList(parseChar '[') (parseChar ')') (parseChar ',') (parseAnyChar " \n\t") parseInt) "[1 \n )yess")
  assertEqual "parseListTest" Nothing (runParser  (parseList(parseChar '[') (parseChar ')') (parseChar ',') (parseAnyChar " \n\t") parseInt) "[1 \n 1)yess")
  assertEqual "parseListTest" Nothing (runParser  (parseList(parseChar '(') (parseChar ')') (parseChar ' ') (parseChar ' ') parseInt) "(  3f )")
  assertEqual "parseListTest" (Just ([[1,2],[1,2]], "")) (runParser  (parseList (parseChar '(') (parseChar ')') (parseChar ' ') (parseChar ' ') (parseList (parseChar '(') (parseChar ')') (parseChar ' ') (parseChar ' ') parseInt)) "(  (1  2 ) ( 1 2) )" )

tests :: Test
tests =
  TestList
    [ TestLabel "ParseCharTest" parseCharTest
    , TestLabel "ParseStringTest" parseStringTest
    , TestLabel "ParseAnyCharTest" parseAnyCharTest
    , TestLabel "ParseOrTest" parseOrTest
    , TestLabel "ParseAndTest" parseAndTest
    , TestLabel "ParseAndWithTest" parseAndWithTest
    , TestLabel "ParseManyTest" parseManyTest
    , TestLabel "ParseSomeTest" parseSomeTest
    , TestLabel "ParseUIntTest" parseUIntTest
    , TestLabel "ParseIntTest" parseIntTest
    , TestLabel "ParseListTest" parseListTest
    ]

main :: IO ()
main = do
  putStrLn "Running tests..."
  _ <- runTestTT tests
  putStrLn "Done."
