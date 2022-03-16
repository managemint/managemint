module TomlishParser where

import Data.Maybe
import Data.Bifunctor

import Control.Applicative

-- - Empty Lines?
-- - ignore spaces
--
-- <Start>   ::= <Start'> | e
-- <Start'>  ::= <Line> \n <Start'> | <Line>
-- <Line>    ::= [ <Path> ] | <Key> = <Value> | <Comment>
-- <Path>    ::= <Key>.<Path> | <Key>
-- <Value>   ::= <QuotedString> | <Int>
-- <Comment> ::= #<String>
-- <Key>     ::= Alphanumeric String, no spaces
-- <QuotedString>  ::= "<String>"
-- <String>  ::= List of Characters
-- <Int>     ::= Integral Value

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
