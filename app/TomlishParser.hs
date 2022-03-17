module TomlishParser where

import Data.Maybe
import Data.Bifunctor

import Control.Applicative
import Control.Monad

import Data.Char

-- - Empty Lines?
-- - ignore spaces
--
-- TODO: comments, support more types as values
-- <Top>     ::= <Segment> \n <Top> | <Top> | e
-- <Segment> ::= [ <Path> ] \n <Node> | [ <Path> ]
-- <Node>    ::= <Key> = <Value> \n <Node> | <Key> = <Value>
-- <Path>    ::= <Key>.<Path> | <Key>
-- <Key>     ::= Alphanumeric String (No Spaces)
-- <Value>   ::= Integer Value | <Quoted String>
-- <Quoted String> ::= "<String|c/='"'>"
-- "<String ohne quotes>

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> map (first f) $ p s

instance Applicative Parser where
    pure x = Parser $ \s -> [(x,s)]
    (Parser p1) <*> (Parser p2) = Parser $ \inp ->
        [(r1 r2, rem2) | (r1,rem1) <- p1 inp, (r2,rem2) <- p2 rem1]

instance Alternative Parser where
    empty = Parser $ const []
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s ++ p2 s


runParser :: Parser a -> String -> [(a,String)]
runParser (Parser p) = p

runParserComplete :: Parser a -> String -> [a]
runParserComplete (Parser p) s = [ r | (r,"") <- p s ]

parse :: Parser a -> String -> Maybe a
parse p s = listToMaybe $ runParserComplete p s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser check
    where
        check (c:s) | p c = [(c,s)]
        check   _         = [     ]

-- satisfyAll :: [(Char -> Bool)] -> Parser Char
-- satisfyAll ps = fold satisfy ps

char :: Char -> Parser Char
char c = satisfy (c ==)

notChar :: Char -> Parser Char
notChar c = satisfy (c /=)

notChars :: [Char] -> Parser Char
notChars cs = satisfy(`notElem` cs)

space :: Parser Char
space = satisfy isSpace

alpha :: Parser Char
alpha = satisfy isAlpha

newLine :: Parser Char
newLine = char '\n'

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

line :: Parser String
line = many $ notChars "\"\n"

alphaNum :: Parser String
alphaNum = some $ satisfy isAlphaNum

keyword :: String -> Parser String
keyword = traverse char

skipSpaces :: Parser ()
skipSpaces = () <$ many space

skipNewline :: Parser ()
skipNewline = () <$ many (char '\n' <|> space)

natural :: Parser Int
natural = accum <$> some digit
    where
        accum = foldl (\a n-> n + a*10) 0

integral :: Parser Int
integral = natural
        <|> (\i -> (-1)*i) <$> (char '-' *> natural)

data TomlishType= TomlishString String
                | TomlishInt Int
                deriving (Show)

data Tree a b   = Node a [Tree a b]
                | Leave a b
                deriving (Show)

type TomlishTree = Tree String TomlishType

addLeavesLinear :: TomlishTree -> [TomlishTree] -> TomlishTree
addLeavesLinear (Node k []) b = Node k b
addLeavesLinear (Node k [t]) b = Node k [addLeavesLinear t b]
addLeavesLinear _ _ = error "addLeavesLinear undefined behaviour"

-- addLeavesLinear (Node k (t:ts)) b = undefined
-- addLeavesLinear (Leave _ _) _ = undefined

parseValue :: Parser TomlishType
parseValue = TomlishInt <$> integral
          <|> TomlishString <$> (char '"' *> line <* char '"')
          <|> TomlishString <$> (char '\'' *> line <* char '\'')

parseKey :: Parser TomlishTree
parseKey =  (`Node` []) <$> alphaNum

parsePath :: Parser TomlishTree
parsePath =  parseKey
         <|> (\s t -> Node s [t]) <$> alphaNum <* char '.' <*> parsePath

parseKeyValue :: Parser TomlishTree
parseKeyValue =  Leave <$> (alphaNum <* skipSpaces <* char '=') <*> (skipSpaces *> parseValue)


parseNode :: Parser [TomlishTree]
parseNode =  (:[]) <$> parseKeyValue
         <|> (:) <$> (parseKeyValue <* skipNewline <* char '\n') <*> parseNode

parseSegmentTop :: Parser TomlishTree
parseSegmentTop =  char '[' *> skipSpaces *> parsePath <* skipSpaces <* char ']'

parseSegment :: Parser TomlishTree
parseSegment =  parseSegmentTop
            <|> addLeavesLinear <$> (parseSegmentTop <* skipNewline <* char '\n') <*> parseNode

parseTop :: Parser [TomlishTree]
parseTop =  (:[]) <$> parseSegment <* skipNewline
        <|> (:) <$> (parseSegment <* skipNewline <* char '\n') <*> parseTop

