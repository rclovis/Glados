module TestAst (astTestSuite) where

import Ast.Ast (Ast (..), genAst)
import Ast.Expr (genExpr)
import Ast.Op
import Ast.Types
import Data.Char
import Lexer (tokenize)
import Test.HUnit

astTestSuite :: Test
astTestSuite =
  TestList
    [ defineTestSuite
    ]

-- -- Test Suites
------------------------------------------------------------------------

defineTestSuite :: Test
defineTestSuite =
  TestList
    [ vDefineI32,
      vDefineI64,
      vDefineArray,
      vDefineArrayNoSize,
      vDefineArrayBiggerSize,
      vDefineString,
      vDefineStringNoSize,
      vDefineStringBiggerSize,
      vDefineStringConcat,
      vDefineExpresion_1,
      vDefineExpresion_2,
      vDefineExpresion_3,
      vDefineFunctionNoArgs,
      vDefineFunctionOneArgs,
      vDefineFunctionTwoArgs,
      fDefineArrayWrongSize_1,
      fDefineArrayWrongSize_2,
      fDefineArrayWrongSize_3,
      fDefineWrongType,
      fDefineNoVar,
      fDefineNoType,
      fDefineNoName,
      fDefineNoValue,
      fDefineNoSemicolon
    ]

-- Define Tests
------------------------------------------------------------------------

vDefineI32 :: Test
vDefineI32 = do
  let input = "var n: i32 = 10;"
  let expected = Just $ Define "n" Ti32 (Int 10)
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineI32" expected actual)

vDefineI64 :: Test
vDefineI64 = do
  let input = "var n: i64 = 10;"
  let expected = Just $ Define "n" Ti64 (Int 10)
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineI64" expected actual)

vDefineArray :: Test
vDefineArray = do
  let input = "var n: i32[5] = [10, 20, 30, 40, 50];"
  let expected = Just $ Define "n" Tu64 (Array Ti32 5 (ArrayValue [Int 10, Int 20, Int 30, Int 40, Int 50]))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineArray" expected actual)

vDefineArrayNoSize :: Test
vDefineArrayNoSize = do
  let input = "var n: i32[] = [10, 20, 30, 40, 50];"
  let expected = Just $ Define "n" Tu64 (Array Ti32 5 (ArrayValue [Int 10, Int 20, Int 30, Int 40, Int 50]))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineArrayNoSize" expected actual)

vDefineArrayBiggerSize :: Test
vDefineArrayBiggerSize = do
  let input = "var n: i32[5] = [10, 20, 30];"
  let expected = Just $ Define "n" Tu64 (Array Ti32 5 (ArrayValue [Int 10, Int 20, Int 30, Int 0, Int 0]))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineArrayBiggerSize" expected actual)

vDefineString :: Test
vDefineString = do
  let input = "var n: u8[6] = \"hello\";"
  let expected = Just $ Define "n" Tu64 (Array Tu8 6 (ArrayValue [Int (ord 'h'), Int (ord 'e'), Int (ord 'l'), Int (ord 'l'), Int (ord 'o'), Int 0]))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineString" expected actual)

vDefineStringNoSize :: Test
vDefineStringNoSize = do
  let input = "var n: u8[] = \"hello world!\";"
  let expected = Just $ Define "n" Tu64 (Array Tu8 13 (ArrayValue [Int (ord 'h'), Int (ord 'e'), Int (ord 'l'), Int (ord 'l'), Int (ord 'o'), Int (ord ' '), Int (ord 'w'), Int (ord 'o'), Int (ord 'r'), Int (ord 'l'), Int (ord 'd'), Int (ord '!'), Int 0]))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineStringNoSize" expected actual)

vDefineStringBiggerSize :: Test
vDefineStringBiggerSize = do
  let input = "var n: u8[10] = \"hello!\";"
  let expected = Just $ Define "n" Tu64 (Array Tu8 10 (ArrayValue [Int (ord 'h'), Int (ord 'e'), Int (ord 'l'), Int (ord 'l'), Int (ord 'o'), Int (ord '!'), Int 0, Int 0, Int 0, Int 0]))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineStringBiggerSize" expected actual)

vDefineStringConcat :: Test
vDefineStringConcat = do
  let input = "var n: u8[] = \"hello\" \"world!\";"
  let expected = Just $ Define "n" Tu64 (Array Tu8 12 (ArrayValue [Int (ord 'h'), Int (ord 'e'), Int (ord 'l'), Int (ord 'l'), Int (ord 'o'), Int (ord 'w'), Int (ord 'o'), Int (ord 'r'), Int (ord 'l'), Int (ord 'd'), Int (ord '!'), Int 0]))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineStringConcat" expected actual)

vDefineExpresion_1 :: Test
vDefineExpresion_1 = do
  let input = "var n: i32 = 10 + 20;"
  let expected = Just $ Define "n" Ti32 (BinOp Add (Int 10) (Int 20))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineExpresion_1" expected actual)

vDefineExpresion_2 :: Test
vDefineExpresion_2 = do
  let input = "var n: i32 = 10 - f(a);"
  let expected = Just $ Define "n" Ti32 (BinOp Sub (Int 10) (Call "f" [Id "a"]))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineExpresion_2" expected actual)

vDefineExpresion_3 :: Test
vDefineExpresion_3 = do
  let input = "var n: i32 = 10 * a[1];"
  let expected = Just $ Define "n" Ti32 (BinOp Mul (Int 10) (Indexing "a" (Int 1)))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineExpresion_3" expected actual)

vDefineFunctionNoArgs :: Test
vDefineFunctionNoArgs = do
  let input = "funk f(): i32 {return 1;}"
  let expected = Just $ Define "f" Ti32 (Lambda [] (Return (Int 1)))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineFunctionNoArgs" expected actual)

vDefineFunctionOneArgs :: Test
vDefineFunctionOneArgs = do
  let input = "funk f(a: i32): i32 {return 1;}"
  let expected = Just $ Define "f" Ti32 (Lambda [("a", Ti32)] (Return (Int 1)))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineFunctionOneArgs" expected actual)

vDefineFunctionTwoArgs :: Test
vDefineFunctionTwoArgs = do
  let input = "funk f(a: i32, b: i32): i32 {return 1;}"
  let expected = Just $ Define "f" Ti32 (Lambda [("a", Ti32), ("b", Ti32)] (Return (Int 1)))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vDefineFunctionTwoArgs" expected actual)

fDefineArrayWrongSize_1 :: Test
fDefineArrayWrongSize_1 = do
  let input = "var n: i32[10.0] = [10, 20, 30];"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineArrayWrongSize" expected actual)

fDefineArrayWrongSize_2 :: Test
fDefineArrayWrongSize_2 = do
  let input = "var n: i32[10 + 1] = [10, 20, 30];"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineArrayWrongSize" expected actual)

fDefineArrayWrongSize_3 :: Test
fDefineArrayWrongSize_3 = do
  let input = "var n: i32[a] = [10, 20, 30];"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineArrayWrongSize" expected actual)

fDefineWrongType :: Test
fDefineWrongType = do
  let input = "var n: i32 = 10.0;"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineWrongType" expected actual)

fDefineNoType :: Test
fDefineNoType = do
  let input = "var n = 10;"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineNoType" expected actual)

fDefineNoName :: Test
fDefineNoName = do
  let input = "var : i32 = 10;"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineNoName" expected actual)

fDefineNoValue :: Test
fDefineNoValue = do
  let input = "var n: i32 = ;"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineNoValue" expected actual)

fDefineNoSemicolon :: Test
fDefineNoSemicolon = do
  let input = "var n: i32 = 10"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineNoSemicolon" expected actual)

fDefineNoVar :: Test
fDefineNoVar = do
  let input = "n: i32 = 10;"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineNoVar" expected actual)
