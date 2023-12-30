module Lexer
  ( Token (..),
    tokenize,
    -- mainTokenize,
  )
where

import Control.Applicative (Alternative (..))
import Parser
  ( Parser (..),
    parseAnd,
    parseAndWith,
    parseAnyChar,
    parseChar,
    parseInt,
    parseList,
    parseMany,
    parseNothing,
    parseOr,
    parseSome,
    parseString,
    parseUInt,
    runParser,
    parseQuantity,
    parseFloat,
    parseUFloat,
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


data Token
  = ClosePar
  | OpenPar
  | CloseBracket
  | OpenBracket
  | CloseBrace
  | OpenBrace
  | Include
  | Var
  | If
  | Else
  | While
  | Break
  | Funk
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
parseType = fmap Type (fmap (const I8) (parseString "i8") <|> fmap (const I16) (parseString "i16") <|> fmap (const I32) (parseString "i32") <|> fmap (const I64) (parseString "i64") <|> fmap (const F32) (parseString "f32") <|> fmap (const F64) (parseString "f64") <|> fmap (const ISize) (parseString "isize") <|> fmap (const U8) (parseString "u8") <|> fmap (const U16) (parseString "u16") <|> fmap (const U32) (parseString "u32") <|> fmap (const U64) (parseString "u64") <|> fmap (const USize) (parseString "usize") <|> fmap (const Bool) (parseString "bool"))

parseFunk :: Parser Token
parseFunk = fmap (const Funk) (parseString "funk")

parseInclude :: Parser Token
parseInclude = fmap (const Include) (parseString "include")

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
parseToken = parseComment <|> parseClosePar <|> parseOpenPar <|> parseCloseBracket <|> parseOpenBracket<|> parseCloseBrace <|> parseOpenBrace <|> parseWhile <|> parseIf <|> parseElse <|> parseVar <|> parseInclude <|> parseFunk <|> parseBreak <|> parseType <|> parseIdentifier <|> parseBoolean <|> parsefNumber <|> parseiNumber <|> parseStringLex <|> parseSimpleSymbol <|> parseSymbol

tokenize :: String -> Maybe [Token]
tokenize s = case runParser (parseList parseNothing parseNothing parseNothing (parseAnyChar " \t\n") parseToken) s of
  Just (t, "") -> Just t
  _ -> Nothing
