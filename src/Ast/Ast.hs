{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ast.Ast
  ( Ast (..),
    Arg,
    genAst,
  )
where

import Ast.Expr (Expr (..))
import qualified Ast.Expr as Expr
import Ast.Op (Op (..))
import Ast.Types (Type (..))
import Ast.Utils (takeUntil)
import Control.Applicative
import Data.Char (ord)
import Lexer (OperatorParsed (..), Token (..), TypeParsed (..))

type Arg = (String, Type)

type Size = Int

data Ast
  = Seq [Ast]
  | Allocate String Type Ast
  | Define String Type Ast
  | Assign String Ast
  | AssignArray String Ast Ast
  | Lambda [Arg] Ast
  | Call String [Ast]
  | Return Ast
  | Write Ast
  | Read String
  | Malloc Ast
  | Free Ast
  | Exit Ast
  | If Ast Ast Ast
  | While Ast Ast
  | Break
  | Continue
  | Id String
  | BinOp Op Ast Ast
  | UnOp Op Ast
  | Int Int
  | Float Float
  | Array Type Size Ast
  | ArrayValue [Ast]
  | Indexing String Ast
  deriving (Eq, Ord, Show)

genAst :: [Expr] -> Maybe Ast
genAst xs = do
  (ast, []) <- getAst xs
  pure ast

getAst :: [Expr] -> Maybe (Ast, [Expr])
getAst [] = Nothing
getAst xs = do
  (ast, expr) <-
    getIf xs
      <|> getFunk xs
      <|> getCall xs
      <|> getIf xs
      <|> getWhile xs
      <|> getBreak xs
      <|> getContinue xs
      <|> getDefine xs
      <|> getAssign xs
      <|> getAllocation xs
      <|> getReturn xs
      <|> getKeywordArg xs
      <|> case getValue [Parenthesis xs] of
        Nothing -> Nothing
        Just ast -> Just (ast, [])
  case getAst expr of
    Nothing -> pure (ast, expr)
    Just (Seq ys, zs) -> pure (Seq (ast : ys), zs)
    Just (y, zs) -> pure (Seq [ast, y], zs)

getAllocation :: [Expr] -> Maybe (Ast, [Expr])
getAllocation (A Lexer.Malloc : A (Identifier name) : A (Symbol ":") : A (Lexer.Type t) : A (Symbol "=") : xs) = do
  (value, xs') <- getValueEnd xs
  t' <- getType t
  pure (Ast.Ast.Allocate name t' value, xs')
getAllocation _ = Nothing

getValue :: [Expr] -> Maybe Ast
getValue xs =
  do
    getBinOp lv0priority xs
    <|> getBinOp lv1priority xs
    <|> getBinOp lv2priority xs
    <|> getBinOp lv3priority xs
    <|> getParentheses xs
    <|> getCallFunk xs
    <|> getIndexing xs
    <|> getArrayValue xs
    <|> getNumber xs
    <|> getIdentifier xs
  where
    getCallFunk :: [Expr] -> Maybe Ast
    getCallFunk [FuncCall name (Parenthesis [])] = pure (Call name [])
    getCallFunk [FuncCall name (Parenthesis args)] = do
      ArrayValue args' <- getArrayValue [Brackets args]
      pure (Call name args')
    getCallFunk _ = Nothing

getBinOp :: (Expr -> Bool) -> [Expr] -> Maybe Ast
getBinOp f xs = do
  (a, A (Operator op), b) <- getSplitAtOp f xs
  a' <- getValue a
  b' <- getValue b
  op' <- getOp op
  pure (BinOp op' b' a')

getParentheses :: [Expr] -> Maybe Ast
getParentheses [Parenthesis body] = getValue $ reverse body
getParentheses _ = Nothing

getIndexing :: [Expr] -> Maybe Ast
getIndexing (Expr.Indexing name (Brackets expr) : _) = do
  expr' <- (getValue . reverse) expr
  pure (Ast.Ast.Indexing name expr')
getIndexing _ = Nothing

getSplitAtOp :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
getSplitAtOp f (x : xs)
  | f x = Just ([], x, xs)
  | otherwise = do
      (ys, op, zs) <- getSplitAtOp f xs
      pure (x : ys, op, zs)
getSplitAtOp _ _ = Nothing

lv3priority :: Expr -> Bool
lv3priority (A (Operator Lexer.Mul)) = True
lv3priority (A (Operator Lexer.Div)) = True
lv3priority (A (Operator Lexer.Mod)) = True
lv3priority _ = False

lv2priority :: Expr -> Bool
lv2priority (A (Operator Lexer.Add)) = True
lv2priority (A (Operator Lexer.Sub)) = True
lv2priority _ = False

lv1priority :: Expr -> Bool
lv1priority (A (Operator Lexer.Equal)) = True
lv1priority (A (Operator Lexer.NotEqual)) = True
lv1priority (A (Operator Lexer.Less)) = True
lv1priority (A (Operator Lexer.Greater)) = True
lv1priority (A (Operator Lexer.LessEqual)) = True
lv1priority (A (Operator Lexer.GreaterEqual)) = True
lv1priority _ = False

lv0priority :: Expr -> Bool
lv0priority (A (Operator Lexer.And)) = True
lv0priority (A (Operator Lexer.Or)) = True
lv0priority _ = False

getUnOp :: [Expr] -> Maybe (Ast, [Expr])
getUnOp (A (Operator op) : a : xs) = do
  (a', []) <- getAst [a]
  op' <- Ast.Ast.getOp op
  pure (UnOp op' a', xs)
getUnOp _ = Nothing

getFunk :: [Expr] -> Maybe (Ast, [Expr])
getFunk (A Funk : A (Identifier name) : Parenthesis args : A (Symbol ":") : A (Lexer.Type t) : Braces body : xs) = do
  args' <- case args of
    [] -> pure []
    _ -> mapM getArg (splitBySeparator (== A Comma) args)
  (body', []) <- getAst body
  type' <- getType t
  pure (Define name type' (Lambda args' body'), xs)
  where
    getArg :: [Expr] -> Maybe Arg
    getArg [] = Nothing
    getArg [A (Identifier name'), A (Symbol ":"), A (Lexer.Type t')] = do
      t'' <- getType t'
      pure (name', t'')
    getArg _ = Nothing
getFunk _ = Nothing

getIf :: [Expr] -> Maybe (Ast, [Expr])
getIf (A Lexer.If : Parenthesis cond : Braces thenBody : A Lexer.Else : Braces elseBody : xs) = do
  (cond', []) <- getAst cond
  (thenBody', []) <- getAst thenBody
  (elseBody', []) <- getAst elseBody
  pure (Ast.Ast.If cond' thenBody' elseBody', xs)
getIf (A Lexer.If : Parenthesis cond : Braces body : xs) = do
  (cond', []) <- getAst cond
  (body', []) <- getAst body
  pure (Ast.Ast.If cond' body' (Seq []), xs)
getIf _ = Nothing

getCall :: [Expr] -> Maybe (Ast, [Expr])
getCall (FuncCall name (Parenthesis []) : A End : xs) = pure (Call name [], xs)
getCall (FuncCall name (Parenthesis args) : A End : xs) = do
  ArrayValue args' <- getArrayValue [Brackets args]
  pure (Call name args', xs)
getCall (FuncCall name (Parenthesis []) : xs) = pure (Call name [], xs)
getCall (FuncCall name (Parenthesis args) : xs) = do
  ArrayValue args' <- getArrayValue args
  pure (Call name args', xs)
getCall _ = Nothing

getWhile :: [Expr] -> Maybe (Ast, [Expr])
getWhile (A Lexer.While : Parenthesis cond : Braces body : xs) = do
  (cond', []) <- getAst cond
  (body', []) <- getAst body
  pure (Ast.Ast.While cond' body', xs)
getWhile _ = Nothing

getBreak :: [Expr] -> Maybe (Ast, [Expr])
getBreak (A Lexer.Break : xs) = pure (Ast.Ast.Break, xs)
getBreak _ = Nothing

getContinue :: [Expr] -> Maybe (Ast, [Expr])
getContinue (A Lexer.Continue : xs) = pure (Ast.Ast.Continue, xs)
getContinue _ = Nothing

getArrayValue :: [Expr] -> Maybe Ast
getArrayValue [Brackets expr] = do
  let value = splitBySeparator (== A Comma) expr
  expr' <- mapM (getValue . reverse) value
  pure (ArrayValue expr')
getArrayValue _ = Nothing

getDefine :: [Expr] -> Maybe (Ast, [Expr])
getDefine (A Lexer.Var : A (Identifier name) : A (Symbol ":") : A (Lexer.Type t) : Brackets size : A (Symbol "=") : xs) = do
  t' <- getType t
  sz <- getArraySize size
  (ArrayValue value, xs') <- getArr xs
  if sz == 0
    then pure (Define name Tu64 (Array t' (length value) (ArrayValue value)), xs')
    else
      if sz - length value < 0
        then Nothing
        else
          if sz - length value > 0
            then pure (Define name Tu64 (Array t' sz (ArrayValue (value ++ replicate (sz - length value) (defaultValue t')))), xs')
            else pure (Define name Tu64 (Array t' sz (ArrayValue value)), xs')
getDefine (A Lexer.Var : A (Identifier name) : A (Symbol ":") : A (Lexer.Type t) : A (Symbol "=") : xs) = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  t' <- getType t
  pure (Define name t' expr, xs')
getDefine _ = Nothing

getArr :: [Expr] -> Maybe (Ast, [Expr])
getArr expr = do
  let (expr', xs) = takeUntil (== A End) expr
  arr <-
    getArrayValue expr'
      <|> (stringToArray =<< (\s -> Just (s ++ "\0")) =<< escape =<< getStrings expr')
  pure (arr, xs)
  where
    stringToArray :: String -> Maybe Ast
    stringToArray [] = pure (ArrayValue [])
    stringToArray (c : cs) = do
      ArrayValue arr <- stringToArray cs
      pure (ArrayValue (Int (ord c) : arr))
    escape :: String -> Maybe String
    escape [] = pure []
    escape ('\\' : 'n' : cs) = do
      cs' <- escape cs
      pure ('\n' : cs')
    escape ('\\' : 't' : cs) = do
      cs' <- escape cs
      pure ('\t' : cs')
    escape ('\\' : '\\' : cs) = do
      cs' <- escape cs
      pure ('\\' : cs')
    escape ('\\' : '"' : cs) = do
      cs' <- escape cs
      pure ('"' : cs')
    escape ('\\' : '0' : cs) = do
      cs' <- escape cs
      pure ('\0' : cs')
    escape (c : cs) = do
      cs' <- escape cs
      pure (c : cs')

getStrings :: [Expr] -> Maybe String
getStrings [] = pure ""
getStrings (A (String s) : xs) = do
  s' <- getStrings xs
  pure (s ++ s')
getStrings _ = Nothing

getArraySize :: [Expr] -> Maybe Int
getArraySize [] = pure 0
getArraySize [A (INumber n)] = pure n
getArraySize _ = Nothing

splitBySeparator :: (a -> Bool) -> [a] -> [[a]]
splitBySeparator f as = case break f as of
  (as', []) -> [as']
  (as', _ : ys) -> as' : splitBySeparator f ys

defaultValue :: Type -> Ast
defaultValue Tf32 = Float 0
defaultValue Tf64 = Float 0
defaultValue _ = Int 0

getAssign :: [Expr] -> Maybe (Ast, [Expr])
getAssign (Expr.Indexing name (Brackets index) : A (Symbol "=") : xs) = do
  (value, xs') <- getArr xs <|> getValueEnd xs
  index' <- getValue $ reverse index
  pure (AssignArray name index' value, xs')
getAssign (A (Identifier name) : A (Symbol "=") : xs) = do
  (value, xs') <- getArr xs <|> getValueEnd xs
  pure (Assign name value, xs')
getAssign _ = Nothing

getValueEnd :: [Expr] -> Maybe (Ast, [Expr])
getValueEnd xs = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  pure (expr, xs')

getReturn :: [Expr] -> Maybe (Ast, [Expr])
getReturn (A (Identifier "return") : xs) = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  pure (Return expr, xs')
getReturn _ = Nothing

getKeywordArg :: [Expr] -> Maybe (Ast, [Expr])
getKeywordArg (A Lexer.Write : xs) = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  pure (Ast.Ast.Write expr, xs')
getKeywordArg (A Lexer.Free : xs) = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  pure (Ast.Ast.Free expr, xs')
getKeywordArg (A Lexer.Exit : xs) = do
  let (value, xs') = takeUntil (== A End) xs
  (expr, _) <- getAst value
  pure (Ast.Ast.Exit expr, xs')
getKeywordArg (A (Identifier "read") : A (Identifier name) : A End : xs) = do
  pure (Ast.Ast.Read name, xs)
getKeywordArg _ = Nothing

getNumber :: [Expr] -> Maybe Ast
getNumber [A (INumber n)] = pure (Int n)
getNumber [A (FNumber n)] = pure (Float n)
getNumber _ = Nothing

getIdentifier :: [Expr] -> Maybe Ast
getIdentifier [A (Identifier name)] = pure (Id name)
getIdentifier _ = Nothing

getOp :: OperatorParsed -> Maybe Ast.Op.Op
getOp Lexer.Add = pure Ast.Op.Add
getOp Lexer.Sub = pure Ast.Op.Sub
getOp Lexer.Mul = pure Ast.Op.Mul
getOp Lexer.Div = pure Ast.Op.Div
getOp Lexer.Mod = pure Ast.Op.Mod
getOp Lexer.Equal = pure Ast.Op.Eq
getOp Lexer.NotEqual = pure Ast.Op.Neq
getOp Lexer.Less = pure Ast.Op.Lt
getOp Lexer.Greater = pure Ast.Op.Gt
getOp Lexer.LessEqual = pure Ast.Op.Le
getOp Lexer.GreaterEqual = pure Ast.Op.Ge
getOp Lexer.And = pure Ast.Op.And
getOp Lexer.Or = pure Ast.Op.Or
getOp Lexer.Not = pure Ast.Op.Not

getType :: TypeParsed -> Maybe Type
getType I8 = pure Ti8
getType I16 = pure Ti16
getType I32 = pure Ti32
getType I64 = pure Ti64
getType U8 = pure Tu8
getType U16 = pure Tu16
getType U32 = pure Tu32
getType U64 = pure Tu64
getType F32 = pure Tf32
getType F64 = pure Tf64
getType _ = Nothing
