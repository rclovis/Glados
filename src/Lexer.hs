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
  )

data Token
  = ClosePar
  | OpenPar
  | Symbol String
  | Number Int
  | String String
  | Boolean Bool
  | Null
  deriving (Show, Eq)

printableChar :: String -> String
-- printableChar charExclude = filter (`notElem` charExclude) [' ' .. '~'] ++ ['\n'] ++ ['\t']
printableChar charExclude = filter (`notElem` charExclude) ([' ' .. '~'] ++ ['\n'] ++ ['\t'])

parseClosePar :: Parser Token
parseClosePar = fmap (const ClosePar) (parseChar ')')

parseOpenPar :: Parser Token
parseOpenPar = fmap (const OpenPar) (parseChar '(')

parseSymbol :: Parser Token
parseSymbol = fmap Symbol (parseSome (parseAnyChar (printableChar ") \t\n\"")))

parseNumber :: Parser Token
parseNumber = fmap Number parseInt

parseStringLex :: Parser Token
parseStringLex = fmap String (parseChar '"' *> parseMany (parseAndWith (\_ y -> y) (parseChar '\\') (parseChar '\"') <|> parseAnyChar (printableChar "\"")) <* parseChar '"')

parseBoolean :: Parser Token
parseBoolean = fmap Boolean (parseOr (fmap (const True) (parseString "#t")) (fmap (const False) (parseString "#f")))

parseComment :: Parser Token
parseComment = fmap (const Null) (parseString ";;" *> parseMany (parseAnyChar (printableChar "\n")) *> parseChar '\n')

parseToken :: Parser Token
parseToken = parseComment <|> parseClosePar <|> parseOpenPar <|> parseBoolean <|> parseNumber <|> parseStringLex <|> parseSymbol

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