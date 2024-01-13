module FTestAst (astTestSuite) where

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
    [ defineTestSuite,
      whileTestSuite,
      ifTestSuite,
      arrayFeatureTestSuite,
      functionTestSuite
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
      fDefineNoVar,
      fDefineNoType,
      fDefineNoName,
      fDefineNoValue
    ]

whileTestSuite :: Test
whileTestSuite =
  TestList
    [ vWhile,
      vWhileContinue,
      vWhileBreak,
      fWhileNoBody,
      fWhileNoCondition
    ]

ifTestSuite :: Test
ifTestSuite =
  TestList
    [ vIf,
      vIfElse,
      fIfNoBody,
      fIfNoCondition,
      fOnlyElse
    ]

arrayFeatureTestSuite :: Test
arrayFeatureTestSuite =
  TestList
    [ vArrayAllFeature
    ]

functionTestSuite :: Test
functionTestSuite =
  TestList
    [ vFunctionCallNoArgs,
      vFunctionCallOneArgs,
      vFunctionCallTwoArgs,
      vFunctionCallExpresion,
      vFunctionCallInExpression
    ]

-- Function Tests
------------------------------------------------------------------------
vFunctionCallNoArgs :: Test
vFunctionCallNoArgs = do
  let input = "f();"
  let expected = Just $ Call "f" []
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vFunctionCallNoArgs" expected actual)

vFunctionCallOneArgs :: Test
vFunctionCallOneArgs = do
  let input = "f(10);"
  let expected = Just $ Call "f" [Int 10]
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vFunctionCallOneArgs" expected actual)

vFunctionCallTwoArgs :: Test
vFunctionCallTwoArgs = do
  let input = "f(10, 20);"
  let expected = Just $ Call "f" [Int 10, Int 20]
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vFunctionCallTwoArgs" expected actual)

vFunctionCallExpresion :: Test
vFunctionCallExpresion = do
  let input = "f(10 + 20);"
  let expected = Just $ Call "f" [BinOp Add (Int 10) (Int 20)]
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vFunctionCallExpresion" expected actual)

vFunctionCallInExpression :: Test
vFunctionCallInExpression = do
  let input = "a = f(10 - a * 20);"
  let expected = Just $ Assign "a" (Call "f" [BinOp Sub (Int 10) (BinOp Mul (Id "a") (Int 20))])
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vFunctionCallInExpression" expected actual)

-- Array Tests
------------------------------------------------------------------------
vArrayAllFeature :: Test
vArrayAllFeature = do
  let input =
        "var a: i32[5] = [10, 20, 30];\
        \var b: u8[] = \"hello\" \"world!\";\
        \a = [11, 22, 33, 44, 55];\
        \a[2] = b[2] + 2;\
        \return a[2] + b[0];"
  let expected =
        Just $
          Seq
            [ Define "a" Tu64 (Array Ti32 5 (ArrayValue [Int 10, Int 20, Int 30, Int 0, Int 0])),
              Define "b" Tu64 (Array Tu8 12 (ArrayValue [Int (ord 'h'), Int (ord 'e'), Int (ord 'l'), Int (ord 'l'), Int (ord 'o'), Int (ord 'w'), Int (ord 'o'), Int (ord 'r'), Int (ord 'l'), Int (ord 'd'), Int (ord '!'), Int 0])),
              Assign "a" (ArrayValue [Int 11, Int 22, Int 33, Int 44, Int 55]),
              AssignArray "a" (Int 2) (BinOp Add (Indexing "b" (Int 2)) (Int 2)),
              Return (BinOp Add (Indexing "a" (Int 2)) (Indexing "b" (Int 0)))
            ]
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vArrayAllFeature" expected actual)

-- If Tests
------------------------------------------------------------------------
vIf :: Test
vIf = do
  let input = "if (a < 10) {a = a + 1;}"
  let expected = Just $ If (BinOp Lt (Id "a") (Int 10)) (Assign "a" (BinOp Add (Id "a") (Int 1))) (Seq [])
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vIf" expected actual)

vIfElse :: Test
vIfElse = do
  let input = "if (a < 10) {a = a + 1;} else {a = a - 1;}"
  let expected = Just $ If (BinOp Lt (Id "a") (Int 10)) (Assign "a" (BinOp Add (Id "a") (Int 1))) (Assign "a" (BinOp Sub (Id "a") (Int 1)))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vIfElse" expected actual)

fIfNoBody :: Test
fIfNoBody = do
  let input = "if (a < 10) {}"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vIfNoBody" expected actual)

fIfNoCondition :: Test
fIfNoCondition = do
  let input = "if () {a = a + 1;}"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fIfNoCondition" expected actual)

fOnlyElse :: Test
fOnlyElse = do
  let input = "else {a = a - 1;}"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fOnlyElse" expected actual)

-- While Tests
------------------------------------------------------------------------
vWhile :: Test
vWhile = do
  let input = "while (a < 10) {a = a + 1;}"
  let expected = Just $ While (BinOp Lt (Id "a") (Int 10)) (Assign "a" (BinOp Add (Id "a") (Int 1)))
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vWhile" expected actual)

vWhileContinue :: Test
vWhileContinue = do
  let input = "while (a < 10) {continue}"
  let expected = Just $ While (BinOp Lt (Id "a") (Int 10)) Continue
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vWhileContinue" expected actual)

vWhileBreak :: Test
vWhileBreak = do
  let input = "while (a < 10) {break}"
  let expected = Just $ While (BinOp Lt (Id "a") (Int 10)) Break
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "vWhileBreak" expected actual)

fWhileNoBody :: Test
fWhileNoBody = do
  let input = "while (a < 10) {}"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fWhileNoBody" expected actual)

fWhileNoCondition :: Test
fWhileNoCondition = do
  let input = "while () {a = a + 1;}"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fWhileNoCondition" expected actual)

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

fDefineNoVar :: Test
fDefineNoVar = do
  let input = "n: i32 = 10;"
  let expected = Nothing
  let actual = tokenize input >>= genExpr >>= genAst
  TestCase (assertEqual "fDefineNoVar" expected actual)
