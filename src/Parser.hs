{-# LANGUAGE InstanceSigs #-}

module Parser
  ( Parser (..),
    parseChar,
    parseString,
    parseAnyChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseList,
    parseNothing,
    parseQuantity,
    parseFloat,
    parseUFloat,
    parseAnyCharBut,
    parseAnyCharUntil,
  )
where

import Control.Applicative (Alternative (..))


newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser f'
    where
      f' s = case p s of
        Just (x, s') -> Just (f x, s')
        Nothing -> Nothing

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser f
    where
      f s = Just (x, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser f
    where
      f s = case p1 s of
        Just (x, s') -> case p2 s' of
          Just (y, s'') -> Just (x y, s'')
          Nothing -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
  empty :: Parser a
  empty = Parser f
    where
      f _ = Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser f
    where
      f s = case p1 s of
        Just (x, s') -> Just (x, s')
        Nothing -> p2 s

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p1) >>= f = Parser f'
    where
      f' s = case p1 s of
        Just (x, s') -> runParser (f x) s'
        Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser f
  where
    f (x : xs)
      | x == c = Just (c, xs)
      | otherwise = Nothing
    f [] = Nothing

parseString :: String -> Parser String
parseString s = Parser f
  where
    f xs
      | take len xs == s = Just (s, drop len xs)
      | otherwise = Nothing
    len = length s

parseAnyChar :: String -> Parser Char
parseAnyChar s = Parser f
  where
    f (x : xs)
      | x `elem` s = Just (x, xs)
      | otherwise = Nothing
    f [] = Nothing

parseAnyCharBut :: String -> Parser Char
parseAnyCharBut s = Parser f
  where
    f (x : xs)
      | x `notElem` s = Just (x, xs)
      | otherwise = Nothing
    f [] = Nothing

parseAnyCharUntil :: String -> Parser String
parseAnyCharUntil s = Parser f
  where
    f (x : xs) = case runParser (parseString s) (x : xs) of
      Just _ -> Just ("" , x : xs)
      Nothing -> case runParser (parseAnyCharUntil s) xs of
        Just (xs', s') -> Just (x : xs', s')
        Nothing -> Nothing
    f [] = Just ("", [])

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = p1 <|> p2

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser p1) (Parser p2) = Parser f
  where
    f s = case p1 s of
      Just (x, s') -> case p2 s' of
        Just (y, s'') -> Just ((x, y), s'')
        Nothing -> Nothing
      Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith p0 p1 p2 = Parser f
  where
    f s = case runParser (parseAnd p1 p2) s of
      Just ((a, b), s') -> Just (p0 a b, s')
      Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany (Parser p) = Parser f
  where
    f s = case p s of
      Just (x, s') -> case f s' of
        Just (xs, s'') -> Just (x : xs, s'')
        Nothing -> Nothing
      Nothing -> Just ([], s)

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseQuantity :: Parser a -> Int -> Parser [a]
parseQuantity _ 0 = pure []
parseQuantity (Parser p) n = Parser f
  where
    f s = case p s of
      Just (x, s') -> case runParser (parseQuantity (Parser p) (n - 1)) s' of
        Just (xs, s'') -> Just (x : xs, s'')
        Nothing -> Nothing
      Nothing -> Nothing

parseNothing :: Parser ()
parseNothing = Parser f
  where
    f s = Just ((), s)

parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar ['0' .. '9'])

parseInt :: Parser Int
parseInt = (((negate <$ parseChar '-') <|> (id <$ parseChar '+')) <|> pure id) <*> parseUInt

parseUFloat :: Parser Float
parseUFloat = do
  int <- parseUInt
  _ <- parseChar '.'
  dec <- parseUInt <|> pure 0
  let dec' = fromIntegral dec
  let len = length (show dec)
  return (fromIntegral int + dec' / (10 ^ len))

parseFloat :: Parser Float
parseFloat = (((negate <$ parseChar '-') <|> (id <$ parseChar '+')) <|> pure id) <*> parseUFloat

parseTrim :: Parser a -> Parser b -> Parser a
parseTrim separator trim = do separator <* parseMany trim <|> parseMany trim *> separator <* parseMany trim

parseList :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser [e]
parseList open close sep trim p = do
  _ <- open
  _ <- parseMany trim
  x <- p
  xs <- parseMany (parseAndWith (\_ y -> y) (parseTrim sep trim) p)
  _ <- parseMany trim
  _ <- close
  return (x : xs)
