{-# LANGUAGE InstanceSigs #-}
module Parser
    ( parse
    , Parser(..)
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
    ) where

-- import Data.Maybe (fromJust, isJust)
-- import Text.Read (readMaybe)

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser f'
        where
            f' s = case p s of
                Just (x, s') -> Just (f x, s')
                Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser f
    where
        f (x:xs)
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
        f (x:xs)
            | x `elem` s = Just (x, xs)
            | otherwise = Nothing
        f [] = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser p1) (Parser p2) = Parser f
    where
        f s = case p1 s of
            Just (x, s') -> Just (x, s')
            Nothing -> p2 s

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser p1) (Parser p2) = Parser f
    where
        f s = case p1 s of
            Just (x, s') -> case p2 s' of
                Just (y, s'') -> Just ((x, y), s'')
                Nothing -> Nothing
            Nothing -> Nothing

parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
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
parseSome (Parser p) = Parser f
    where
        f s = case p s of
            Just (x, s') -> case f s' of
                Just (xs, s'') -> Just (x : xs, s'')
                Nothing -> Just ([x], s')
            Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser f
    where
        f s = case runParser (parseSome  (parseAnyChar ['0'..'9'])) s of
            Just (x, s') -> Just (read x :: Int, s')
            Nothing -> Nothing

parseInt :: Parser Int
parseInt = Parser f
    where
        f s = case runParser (parseOr (parseChar '-') (parseChar '+')) s of
            Just (x, s') -> case runParser parseUInt s' of
                Just (y, s'') -> case x of
                    '-' -> Just (-y, s'')
                    '+' -> Just (y, s'')
                    _ -> Nothing
                Nothing -> Nothing
            Nothing -> case runParser parseUInt s of
                Just (y, s') -> Just (y, s')
                Nothing -> Nothing




parseList :: Parser a -> Parser b -> Parser c -> Parser d -> Parser [d]
parseList open close sep p = Parser f
    where
        f s = case runParser open s of
            Just (_, s') -> case runParser (parseMany (parseAndWith (\ _ y -> y) (parseMany sep) p)) s' of
                Just (xs, s'') -> case runParser  (parseAndWith (\ _ y -> y) (parseMany sep) close) s'' of
                    Just (_, s''') -> Just (xs, s''')
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing



parse :: String -> IO()
parse _ = do
    -- Example of using the Parser type
    print $ runParser (parseString "hello") "hello world"

    -- Example of using the parseOr function
    print $ runParser (parseOr (parseString "hello") (parseString "world")) "hello world"

    print $ runParser  (parseAnd ( parseChar 'a') ( parseChar 'b')) "abcd"
    print $ runParser  (parseAnd ( parseChar 'a') ( parseChar 'b')) "bcda"
    print $ runParser  (parseAnd ( parseChar 'a') ( parseChar 'b')) "acd"

    print $ runParser  (parseAndWith (\ x y -> [x, y]) ( parseChar 'a') ( parseChar 'b')) "abcd"

    print " "


    print $ runParser  (parseMany ( parseChar ' ')) "     foobar"
    print $ runParser  (parseMany ( parseChar ' ')) "      foobar"
    print $ runParser  (parseMany ( parseChar ' ')) " foobar"
    print $ runParser  (parseMany ( parseChar ' ')) "  foobar"
    print $ runParser  (parseMany ( parseChar ' ')) "foobar  "
    print $ runParser  (parseMany ( parseChar ' ')) "  "
    print $ runParser  (parseMany ( parseChar ' ')) " "


    print " "


    print $ runParser  (parseSome ( parseChar ' ')) "     foobar"
    print $ runParser  (parseSome ( parseChar ' ')) "      foobar"
    print $ runParser  (parseSome ( parseChar ' ')) " foobar"
    print $ runParser  (parseSome ( parseChar ' ')) "  foobar"
    print $ runParser  (parseSome ( parseChar ' ')) "foobar  "
    print $ runParser  (parseSome ( parseChar ' ')) "  "
    print $ runParser  (parseSome ( parseChar ' ')) " "

    print " "

    print $ runParser  (parseList ( parseChar '(') ( parseChar ')') ( parseChar ' ') parseInt) "(  1  2 3 4123 555 )"
