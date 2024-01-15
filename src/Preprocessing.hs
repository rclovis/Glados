module Preprocessing
  ( preprocessing
  )
where
import Parser

data DependenceTree = DependenceTree
  { name :: String,
    content :: String,
    children :: [DependenceTree]
  }
  deriving (Show, Eq)

parseInclude :: Parser String
parseInclude = do
  _ <- parseString "#include"
  _ <- parseMany (parseAnyChar " \t")
  _ <- parseChar '"'
  s <- parseMany (parseAnyCharBut "\"")
  _ <- parseChar '\"'
  return s

parseDefine :: Parser (String, String)
parseDefine = do
  _ <- parseString "#define"
  _ <- parseMany (parseAnyChar " \t")
  nameM <- parseMany (parseAnyCharBut " \t")
  _ <- parseMany (parseAnyChar " \t")
  value <- parseMany (parseAnyCharBut " \t\n")
  return (nameM, value)

initDependenceTree :: [(String, String)] -> [(DependenceTree, [String])]
initDependenceTree [] = []
initDependenceTree ((a, b) : xs) = do
  case runParser (parseAnyCharUntil "#include") b of
    Just (_, s') -> case runParser (parseMany (parseAndWith const parseInclude (parseAnyCharUntil "#include"))) s' of
      Just (s, _) -> (DependenceTree a b [], s) : initDependenceTree xs
      Nothing -> (DependenceTree a b [], []) : initDependenceTree xs
    Nothing -> (DependenceTree a b [], []) : initDependenceTree xs

getFilesContent :: [String] -> IO [(String, String)]
getFilesContent [] = return []
getFilesContent (x : xs) = do
  file <- readFile x
  followingFiles <- getFilesContent xs
  return ((x, file) : followingFiles)

correspondindNode :: String -> [(DependenceTree, [String])] -> (DependenceTree, [String])
correspondindNode _ [] = error "Error: file not found"
correspondindNode s ((a, as) : xs) = do
  if name a == s then (a, as) else correspondindNode s xs

buildDependenceTree :: (DependenceTree, [String]) -> [(DependenceTree, [String])] -> [String] -> DependenceTree
buildDependenceTree (a, b) list visited = do
  if name a `elem` visited then error "Error: circular dependency"
  else do
    let children' = map (\x -> buildDependenceTree (correspondindNode x list) list (name a:visited)) b
    let a' = DependenceTree (name a) (content a) children'
    a'

replaceDependence :: String -> Parser String
replaceDependence s = do
  before <- parseAnyCharUntil "#include"
  _ <- parseString "#include"
  _ <- parseMany (parseAnyChar " \t")
  _ <- parseChar '\"'
  _ <- parseMany (parseAnyCharBut "\"")
  _ <- parseChar '\"'
  after <- parseMany (parseAnyCharBut "")
  return (before ++ s ++ after)

replaceDefine :: Parser String
replaceDefine = do
  before <- parseAnyCharUntil "#define"
  _ <- parseString "#define"
  _ <- parseMany (parseAnyChar " \t")
  _ <- parseMany (parseAnyCharBut " \t")
  _ <- parseMany (parseAnyChar " \t")
  _ <- parseMany (parseAnyCharBut " \t\n")
  after <- parseMany (parseAnyCharBut "")
  return (before ++ after)



replaceOccurences :: [(String, String)] -> String -> String
replaceOccurences [] s = s
replaceOccurences xs s = do
  foldr replaceOccurence (replaceDefine2 xs s) xs
  where replaceOccurence (a, b) (x':xs') = case runParser (parseString a) (x':xs') of
          Just (_,xs'') -> b ++ replaceOccurence (a, b) xs''
          Nothing -> x' : replaceOccurence (a, b) xs'
        replaceOccurence _ [] = []

        replaceDefine2 (_:xs') string = case runParser replaceDefine string of
          Just (xs'',_) -> replaceDefine2 xs' xs''
          Nothing -> replaceDefine2 xs' string
        replaceDefine2 [] string = string
-- replaceOccurences :: [(String, String)] -> String -> String
-- replaceOccurences [] s = s
-- replaceOccurences xs s = do
--   let s' = runParser replaceDefine s
--   case s' of
--     Just (s'', _) -> foldr replaceOccurence s'' xs
--     Nothing -> error "Error: #define not found"
--   where replaceOccurence (a, b) (x':xs') = case runParser (parseString a) (x':xs') of
--           Just (_,xs'') -> b ++ replaceOccurence (a, b) xs''
--           Nothing -> x' : replaceOccurence (a, b) xs'
--         replaceOccurence _ [] = []

collapseDependenceTree :: DependenceTree -> String
collapseDependenceTree (DependenceTree _ content1 []) = content1
collapseDependenceTree (DependenceTree n content1 (x:xs)) = do
  let contentC = collapseDependenceTree x
  let c = runParser (replaceDependence contentC) content1
  case c of
    Just (s, _) -> collapseDependenceTree (DependenceTree n s xs)
    Nothing -> error "Error: #include not found"

getDefine :: Parser [(String, String)]
getDefine = parseAnyCharUntil "#define" *> parseMany (parseAndWith const parseDefine (parseAnyCharUntil "#define"))

preprocessing :: [String] -> IO String
preprocessing i = do
  filesContent <- getFilesContent i
  let dependenceTree = initDependenceTree filesContent
  let dependenceTree' = buildDependenceTree (head dependenceTree) dependenceTree []
  let contentMain = collapseDependenceTree dependenceTree'
  let define = runParser getDefine contentMain
  case define of
    Just (s, _) -> do
      let contentMain' = replaceOccurences s contentMain
      return contentMain'
    Nothing -> return contentMain
