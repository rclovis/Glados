module Lexer
  ( Token (..),
    tokenize,
    TypeParsed (..),
    OperatorParsed (..),
    -- mainTokenize,
  )
where

import Control.Applicative (Alternative (..))
import Parser
  ( Parser (..),
    parseAndWith,
    parseAnyChar,
    parseChar,
    parseFloat,
    parseInt,
    parseList,
    parseMany,
    parseNothing,
    parseOr,
    parseQuantity,
    parseSome,
    parseString,
    parseUFloat,
    parseUInt,
    runParser,
  )

data TypeParsed
  = I8
  | I16
  | I32
  | I64
  | F32
  | F64
  | ISize
  | U8
  | U16
  | U32
  | U64
  | USize
  | Bool
  deriving (Show, Eq)

data OperatorParsed
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Not
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  deriving (Show, Eq)

data Token
  = ClosePar
  | OpenPar
  | CloseBracket
  | OpenBracket
  | CloseBrace
  | OpenBrace
  | Include
  | Continue
  | Var
  | If
  | Else
  | While
  | Break
  | Comma
  | End
  | Funk
  | Operator OperatorParsed
  | Type TypeParsed
  | Identifier String
  | Symbol String
  | INumber Int
  | FNumber Float
  | String String
  | Boolean Bool
  | Null
  deriving (Show, Eq)

printableChar :: String -> String
printableChar charExclude = filter (`notElem` charExclude) ([' ' .. '~'] ++ ['\n'] ++ ['\t'])

parseClosePar :: Parser Token
parseClosePar = fmap (const ClosePar) (parseChar ')')

parseOpenPar :: Parser Token
parseOpenPar = fmap (const OpenPar) (parseChar '(')

parseCloseBracket :: Parser Token
parseCloseBracket = fmap (const CloseBracket) (parseChar ']')

parseOpenBracket :: Parser Token
parseOpenBracket = fmap (const OpenBracket) (parseChar '[')

parseCloseBrace :: Parser Token
parseCloseBrace = fmap (const CloseBrace) (parseChar '}')

parseOpenBrace :: Parser Token
parseOpenBrace = fmap (const OpenBrace) (parseChar '{')

parseType :: Parser Token
parseType =
  fmap
    Type
    ( fmap (const I8) (parseString "i8")
        <|> fmap (const I16) (parseString "i16")
        <|> fmap (const I32) (parseString "i32")
        <|> fmap (const I64) (parseString "i64")
        <|> fmap (const F32) (parseString "f32")
        <|> fmap (const F64) (parseString "f64")
        <|> fmap (const ISize) (parseString "isize")
        <|> fmap (const U8) (parseString "u8")
        <|> fmap (const U16) (parseString "u16")
        <|> fmap (const U32) (parseString "u32")
        <|> fmap (const U64) (parseString "u64")
        <|> fmap (const USize) (parseString "usize")
        <|> fmap (const Bool) (parseString "bool")
    )

parseOperator :: Parser Token
parseOperator =
  fmap
    Operator
    ( fmap (const Mul) (parseString "* ")
        <|> fmap (const And) (parseString "&&")
        <|> fmap (const Or) (parseString "||")
        <|> fmap (const Equal) (parseString "==")
        <|> fmap (const NotEqual) (parseString "!=")
        <|> fmap (const LessEqual) (parseString "<=")
        <|> fmap (const GreaterEqual) (parseString ">=")
        <|> fmap (const Add) (parseChar '+')
        <|> fmap (const Sub) (parseChar '-')
        <|> fmap (const Div) (parseChar '/')
        <|> fmap (const Mod) (parseChar '%')
        <|> fmap (const Not) (parseChar '!')
        <|> fmap (const Greater) (parseChar '>')
        <|> fmap (const Less) (parseChar '<')
    )

parseFunk :: Parser Token
parseFunk = fmap (const Funk) (parseString "funk")

parseVar :: Parser Token
parseVar = fmap (const Var) (parseString "var")

parseIf :: Parser Token
parseIf = fmap (const If) (parseString "if")

parseElse :: Parser Token
parseElse = fmap (const Else) (parseString "else")

parseWhile :: Parser Token
parseWhile = fmap (const While) (parseString "while")

parseBreak :: Parser Token
parseBreak = fmap (const Break) (parseString "break")

parseComma :: Parser Token
parseComma = fmap (const Comma) (parseChar ',')

parseContinue :: Parser Token
parseContinue = fmap (const Continue) (parseString "continue")

parseEnd :: Parser Token
parseEnd = fmap (const End) (parseChar ';')

parseIdentifier :: Parser Token
parseIdentifier = fmap Identifier (parseAndWith (++) (parseQuantity (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'])) 1) (parseMany (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']))))

parseSimpleSymbol :: Parser Token
parseSimpleSymbol = fmap Symbol (parseQuantity (parseAnyChar "+-*/%{}[]().;:") 1)

parseSymbol :: Parser Token
parseSymbol = fmap Symbol (parseSome (parseAnyChar (printableChar ") \t\n\"+-*/%{}[]().;:")))

parseiNumber :: Parser Token
parseiNumber = fmap INumber parseInt

parsefNumber :: Parser Token
parsefNumber = fmap FNumber parseFloat

parseStringLex :: Parser Token
parseStringLex = fmap String (parseChar '"' *> parseMany (parseAndWith (\_ y -> y) (parseChar '\\') (parseChar '\"') <|> parseAnyChar (printableChar "\"")) <* parseChar '"')

parseBoolean :: Parser Token
parseBoolean = fmap Boolean (parseOr (fmap (const True) (parseString "true")) (fmap (const False) (parseString "false")))

parseComment :: Parser Token
parseComment = fmap (const Null) (parseString ";;" *> parseMany (parseAnyChar (printableChar "\n")) *> parseChar '\n')

parseToken :: Parser Token
parseToken = parseComment <|> parseEnd <|> parseComma <|> parseClosePar <|> parseOpenPar <|> parseCloseBracket <|> parseOpenBracket <|> parseCloseBrace <|> parseOpenBrace <|> parseWhile <|> parseContinue <|> parseIf <|> parseElse <|> parseVar <|> parseFunk <|> parseBreak <|> parseType <|> parseIdentifier <|> parsefNumber <|> parseiNumber <|> parseOperator <|> parseBoolean  <|> parseStringLex <|> parseSimpleSymbol <|> parseSymbol

tokenize :: String -> Maybe [Token]
tokenize s = case runParser (parseList parseNothing parseNothing parseNothing (parseAnyChar " \t\n") parseToken) s of
  Just (t, "") -> Just t
  _ -> Nothing
