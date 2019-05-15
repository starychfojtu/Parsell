{- 

Small base to a potential monadic parser library, inspired by Parsec.
What these library let us is to write parses in highly reusable, composable and type-safe way with
nice, out of the box error reporting.

-}

import Text.Read
import Data.Function
import Data.Char
import Data.Maybe

-- Helper methods

orElse :: Maybe a -> a -> a
orElse (Just a) _ = a
orElse Nothing a = a 

safeGet :: Int -> [a] -> Maybe a
safeGet index arr
    | index < length arr = Just $ arr !! index
    | otherwise = Nothing

-- Data structures

-- Represent a position in text (e.g. reading input)
data Position = Position
    { line :: Int
    , column :: Int
    } deriving Show

startPosition :: Position
startPosition = Position { line = 0, column = 0 }

nextLine :: Position -> Position
nextLine p = Position { line = (line p) + 1, column = 0 }

nextToken :: Position -> Position
nextToken p = p { column = (column p) + 1 }

data Input = Input 
    { input :: [String]
    , position :: Position
    } deriving Show

toInput :: String -> Input
toInput s = 
    Input { input = lines s, position = startPosition }

readNext :: Input -> Maybe (Char, Input)
readNext Input { input = inputLines, position = pos } =
    let colPos = column pos
    in do 
        currentLine <- safeGet (line pos) inputLines
        c <- safeGet colPos currentLine
        let newPosition = if length currentLine == colPos + 1 then nextLine pos else nextToken pos
        return (c, Input { input = inputLines, position = newPosition })

data ParserError = ParserError
    { message :: String
    , columnNumber :: Int
    , lineNumber :: Int
    , currentLine :: String
    }

instance Show ParserError where
    show ParserError { message = msg, columnNumber = columnNb, lineNumber = lineNb, currentLine = l } =
        msg ++ " At line: " ++ show lineNb ++ ", column: " ++ show columnNb ++ ". Line: " ++ l

fromInput :: String -> Input -> ParserError
fromInput message i = ParserError
    { message = message
    , columnNumber = (column . position) i
    , lineNumber = (line . position) i
    , currentLine = safeGet ((line . position) i) (input i) & (flip orElse) ""
    }

data ParserResult a = 
    Success (a, Input) |
    Error ParserError
    deriving Show

data Parser a = Parser
    { parse :: (Input -> ParserResult a)
    , name :: Maybe String
    }

create :: Maybe String -> (Input -> ParserResult a) -> Parser a
create name parse = Parser { parse = parse, name = name }

pureSuccess :: a -> Parser a
pureSuccess a = create Nothing (\s -> Success (a, s))

pureError :: String -> Parser a
pureError e = create Nothing (\i -> Error $ fromInput e i)

setMaybeName :: Maybe String -> Parser a -> Parser a
setMaybeName name p = p { name = name }
        
setName :: String -> Parser a -> Parser a
setName = setMaybeName . Just

-- Combinators

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser p f = create Nothing (\s ->
    case run p s of
        Success (a, rest) -> run (f a) rest
        Error e -> Error e)

instance Functor Parser where
    fmap f p = bindParser p (pureSuccess . f) & setMaybeName (name p)

instance Applicative Parser where
    pure = pureSuccess
    (<*>) funcParser argParser = 
        bindParser funcParser (\f -> fmap f argParser)

instance Monad Parser where
    (>>=) = bindParser

run :: Parser a -> Input -> ParserResult a
run p s = (parse p) s

-- This function serves as user friendly 'run' function
getResult :: Parser a -> String -> Either String a
getResult p s =
    let parserName = orElse (name p) "Unknown"
        result = toInput s & run p
        errorMsg = "Error while parsing " ++ parserName ++ ". "
    in case result of
        Success (a, rest) -> 
            case readNext rest of
                Nothing -> Right a
                Just (c, _) -> Left $ errorMsg ++ show (fromInput ("Unexpected: " ++ [c]) rest)
        Error error -> Left $ errorMsg ++ show error

-- Chains two parsers together
(.>>.) :: Parser a -> Parser b -> Parser (a, b)
p1 .>>. p2 = 
    let p = do
            p1Result <- p1
            p2Result <- p2
            return (p1Result, p2Result)
        newName = (\a b -> a ++ " and " ++ b) <$> (name p1) <*> (name p2)
    in p & setMaybeName newName

-- Chains two parsers, but ignores the output of the second one
(.>>) :: Parser a -> Parser b -> Parser a
(.>>) p1 p2 = p1 .>>. p2 & fmap fst

-- Chains two parsers, but ignores the output of the first one
(>>.) :: Parser a -> Parser b -> Parser b
(>>.) p1 p2 = p1 .>>. p2 & fmap snd

-- This function enables backtracking by specifing an OR relation ship between 2 parsers
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = create ((\a b -> (a ++ " or " ++ b)) <$> (name p1) <*> (name p2)) (\s ->
    case run p1 s of
        Success (p1Result, rest) -> 
            Success (p1Result, rest)
        Error _ -> 
            run p2 s)

-- <|> on a collection of parsers
anyOf :: [Parser a] -> Parser a
anyOf (p:ps) = foldl (<|>) p ps
anyOf _ = error "No parsers provided."

-- traditional sequence combinator ([M a] -> M [a])
seqOfParsers :: [Parser a] -> Parser [a]
seqOfParsers = foldl (\accumulator parser -> accumulator .>>. parser & fmap (\(arr,a) -> arr ++ [a])) $ pure []

zeroOrMore :: Parser a -> Input -> ([a], Input)
zeroOrMore p s =
    case run p s of
        Success (result, rest) -> 
            let (results, rest2) = zeroOrMore p rest 
            in (result:results, rest2)
        Error _ ->
            ([], s)

-- Matches the parser eagearly as many times as possible, similar to '*' in regex
many :: Parser a -> Parser [a]
many p = create (fmap ("Sequence of " ++) (name p)) (\s -> Success $ zeroOrMore p s)

-- Matches the parser eagearly as many times as possible, but at least one time. Similar to '+' in regex
many1 :: Parser a -> Parser [a]
many1 p = p .>>. (many p) & fmap (\(a, as) -> a:as)

-- Optionally matches the parser, which makes it non required
opt :: Parser a -> Parser (Maybe a)
opt p =
    let justParser = fmap Just p
        nothingParser = pure Nothing
    in (justParser <|> nothingParser) & setMaybeName (fmap ("Maybe " ++) (name p))

-- Specific char parsers

-- Base stone function which will parse the char iff the predicate holds for it
firstSatisfies :: (Char -> Bool) -> Parser Char
firstSatisfies predicate = create Nothing (\i ->
     case readNext i of
        Nothing -> Error $ fromInput "End of stream." i
        Just (c, newInput) -> 
            if predicate c
                then Success (c, newInput)
                else Error $ fromInput ("Unexpeceted :'" ++ [c] ++ "'.") i)

-- Parses anything, similar to . in reges
anyChar :: Parser Char
anyChar = firstSatisfies (const True) & setName ("Any char")

-- Parses a specific char
charParser :: Char -> Parser Char
charParser c = firstSatisfies (c ==) & setName ("'" ++ [c] ++ "'")

-- Parses specifics chars
anyOfChars :: [Char] -> Parser Char
anyOfChars chars = fmap charParser chars & anyOf & setName "Character"

-- Parses a anything, but give chars. Similar to [^] in regex
exceptForChars :: [Char] -> Parser Char
exceptForChars chars = firstSatisfies (\c -> not $ elem c chars) & setName "Characters excluded"

-- Parses a specific string
stringParser :: String -> Parser String
stringParser s = fmap charParser s & seqOfParsers & setName "String"

-- Parses a digit
digitParser :: Parser Int
digitParser = firstSatisfies isDigit & fmap digitToInt & setName "Digit"

-- Parses eagarly and integer
integerParser :: Parser Int
integerParser = many1 digitParser & fmap (foldl (\acc i -> 10 * acc + i) 0) & setName "Integer"

{-
    Two examples are presented for usage.
    1) Prefix tree parser
    2) Naive Url parser
-}
-- Prefix tree parser

data Operator = Addition | Subtraction | Division | Product deriving Show
data Tree = Node Tree Operator Tree | Leaf Int deriving Show

parseAddition = const Addition <$> charParser '+'
parseSubtraction = const Subtraction <$> charParser '-'
parseDivision = const Division <$> charParser '/'
parseProduct = const Product <$> charParser '*'

parseOperator = 
    anyOf [
        parseAddition,
        parseSubtraction,
        parseDivision,
        parseProduct
    ] & setName "Operator"

parseSpace = 
    charParser ' '

parseTree = 
    let parseNode = (parseOperator .>> parseSpace .>>. parseTree .>> opt parseSpace .>>. parseTree .>> opt parseSpace) & setName "Node (Node Operator Node)"
        parseLeaf = (integerParser .>> opt parseSpace) & setName "Leaf (Integer)"
    in  (fmap (\((op, left), right) -> Node left op right) parseNode <|> fmap Leaf parseLeaf) & setName "Tree"

test1 = getResult parseTree "+ 5 5" 
test2 = getResult parseTree "+ 5" 
test3 = getResult parseTree "+ 5 5 5"
test4 = getResult parseTree "- * / 15 - 7 + 1 1 3 + 2 + 1 1"

-- Url parser

data Scheme = Http | Https deriving Show
data Auth = Auth String String deriving Show
type Host = String
type Port = Int
type Path = String

data Url =
    Url Scheme (Maybe Auth) Host (Maybe Port) (Maybe Path) deriving Show

parseScheme :: Parser Scheme
parseScheme = 
    anyOf [
        stringParser "https" & fmap (const Https),
        stringParser "http" & fmap (const Http)
    ] & setName "Scheme"

parseUser :: Parser String
parseUser =
    exceptForChars [':'] & many1 & setName "Username"

parsePassword :: Parser String
parsePassword =
    exceptForChars ['@'] & many1 & setName "Password"

parseAuthentication :: Parser Auth
parseAuthentication = 
    (parseUser .>> charParser ':' .>>. parsePassword .>> charParser '@') 
    & fmap (uncurry Auth)
    & setName "Authentication"

parseHost :: Parser Host
parseHost = 
    exceptForChars ['/', ':', '@'] & many1 & setName "Host"

parsePort :: Parser Port
parsePort = 
    charParser ':' >>. integerParser & setName "Port"

parsePath :: Parser Path
parsePath = 
    charParser '/' >>. (many anyChar) & setName "Path"

parseUrl :: Parser Url
parseUrl = 
    (parseScheme .>> stringParser "://" .>>. opt parseAuthentication .>>. parseHost .>>. opt parsePort .>>. opt parsePath)
    & fmap (\((((scheme, auth), host), port), path) -> Url scheme auth host port path)
    & setName "Url"

google = getResult parseUrl "http://google.com"
googleAuth = getResult parseUrl "https://pepa:password@google.com:8080/somePath"
wrong1 = getResult parseUrl "https://pepapassword@google.com:8080/somePath"
wrong2 = getResult parseUrl "foo://pepa:password@google.com:8080/somePath"
wrong3 = getResult parseUrl "https://pepa:password@google.com:portNotANumber/somePath"