-- module Parser
--     ( parse
--     ) where
-- import Data.Maybe (fromJust, isJust)
-- import Text.Read (readMaybe)

-- data Token
--     = OpenParen |
--     CloseParen |
--     Number Int |
--     String String |
--     Symbol String
--     deriving (Show)

-- displayTokens :: [Token] -> IO()
-- displayTokens [] = putStrLn ""
-- displayTokens (x:xs) = do
--         print x
--         displayTokens xs

-- parseStringString :: String -> String
-- parseStringString ('"':_) = ""
-- parseStringString (x:xs) = x : parseStringString xs
-- parseStringString [] = ""

-- parseString :: String -> Maybe Token
-- parseString x = Just (String (parseStringString x))

-- parseAtomString :: String -> String
-- parseAtomString ('(':_) = ""
-- parseAtomString (')':_) = ""
-- parseAtomString (' ':_) = ""
-- parseAtomString ('\t':_) = ""
-- parseAtomString ('\n':_) = ""
-- parseAtomString ('"':_) = ""
-- parseAtomString (x:xs) = x : parseAtomString xs
-- parseAtomString [] = ""

-- parseAtom :: String -> Maybe Token
-- parseAtom x = Just (Symbol (parseAtomString x))

-- lengthMaybe :: Maybe Token -> Int
-- lengthMaybe (Just (Number n)) = length (show n)
-- lengthMaybe (Just (Symbol s)) = length s
-- lengthMaybe (Just (String s)) = length s + 1
-- lengthMaybe _ = 1

-- parseToken :: String -> [Maybe Token]
-- parseToken [] = []
-- parseToken ('(':xs) = Just OpenParen : parseToken xs
-- parseToken (')':xs) = Just CloseParen : parseToken xs
-- parseToken (' ':xs) = parseToken xs
-- parseToken ('\t':xs) = parseToken xs
-- parseToken ('\n':xs) = parseToken xs
-- parseToken ('"':xs) = token : parseToken (drop lenToken xs)
--     where token = parseString xs
--           lenToken = lengthMaybe token
-- parseToken x = token : parseToken (drop lenToken x)
--     where token = parseAtom x
--           lenToken = lengthMaybe token

-- evaluateSymbols :: [Maybe Token] -> [Token]
-- evaluateSymbols [] = []
-- evaluateSymbols (Just (Symbol s):xs) = evaluateSymbol (Symbol s) : evaluateSymbols xs
-- evaluateSymbols (Just x:xs) = x : evaluateSymbols xs
-- evaluateSymbols _ = []

-- evaluateSymbol :: Token -> Token
-- evaluateSymbol (Symbol s) |
--     isJust nbr = Number (fromJust nbr)
--     where nbr = readMaybe s :: Maybe Int
-- evaluateSymbol x = x

-- parse :: String -> IO()
-- parse path = do
--     file <- readFile path
--     let tokens = evaluateSymbols (parseToken file)
--     displayTokens tokens
