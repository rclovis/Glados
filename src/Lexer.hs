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
    parseInteger,
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

data Token
  = ClosePar
  | OpenPar
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

parseSimpleSymbol :: Parser Token
parseSimpleSymbol = fmap Symbol (parseQuantity (parseAnyChar "+-*/%={}[]().;:!") 1)

parseSymbol :: Parser Token
parseSymbol = fmap Symbol (parseSome (parseAnyChar (printableChar ") \t\n\"+-*/%={}[]().;:!")))

parseiNumber :: Parser Token
parseiNumber = fmap INumber parseInt

parsefNumber :: Parser Token
parsefNumber = fmap FNumber parseFloat

parseStringLex :: Parser Token
parseStringLex = fmap String (parseChar '"' *> parseMany (parseAndWith (\_ y -> y) (parseChar '\\') (parseChar '\"') <|> parseAnyChar (printableChar "\"")) <* parseChar '"')

parseBoolean :: Parser Token
parseBoolean = fmap Boolean (parseOr (fmap (const True) (parseString "#t")) (fmap (const False) (parseString "#f")))

parseComment :: Parser Token
parseComment = fmap (const Null) (parseString ";;" *> parseMany (parseAnyChar (printableChar "\n")) *> parseChar '\n')

parseToken :: Parser Token
parseToken = parseComment <|> parseClosePar <|> parseOpenPar <|> parseBoolean <|> parsefNumber <|> parseiNumber <|> parseStringLex <|> parseSimpleSymbol <|> parseSymbol

tokenize :: String -> Maybe [Token]
tokenize s = case runParser (parseList parseNothing parseNothing parseNothing (parseAnyChar " \t\n") parseToken) s of
  Just (t, "") -> Just t
  _ -> Nothing

-- printToken :: Token -> IO ()
-- printToken ClosePar = putStrLn "ClosePar"
-- printToken OpenPar = putStrLn "OpenPar"
-- printToken (Symbol s) = putStrLn ("Symbol " ++ s)
-- printToken (Number n) = putStrLn ("Number " ++ show n)
-- printToken (String s) = putStrLn ("String " ++ s)
-- printToken (Boolean b) = putStrLn ("Boolean " ++ show b)

-- printTokens :: Maybe [Token] -> IO ()
-- printTokens (Just tokens) = mapM_ printToken tokens
-- printTokens Nothing = putStrLn "Nothing"

-- mainTokenize :: IO ()
-- mainTokenize = do
--   file <- readFile "test.rkt"
--   printTokens (tokenize file)
