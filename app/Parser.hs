{- app/Parser.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module Parser where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Bifunctor

-- This parser is based on exercise sheet 11

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

char :: Char -> Parser Char
char c = satisfy (c ==)

notChar :: Char -> Parser Char
notChar c = satisfy (c /=)

notChars :: [Char] -> Parser Char
notChars cs = satisfy (`notElem` cs)

space :: Parser Char
space = satisfy isSpace

alpha :: Parser Char
alpha = satisfy isAlpha

newLine :: Parser Char
newLine = char '\n'

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

alphaNum :: Parser String
alphaNum = some $ satisfy isAlphaNum

keyword :: String -> Parser String
keyword = traverse char

skipSpaces :: Parser ()
skipSpaces = () <$ many space

natural :: Parser Int
natural = accum <$> some digit
    where
        accum = foldl (\a n-> n + a*10) 0

integral :: Parser Int
integral = natural
        <|> (\i -> (-1)*i) <$> (char '-' *> natural)

hsVarName :: Parser String
hsVarName = (:) <$> satisfy isLower <*> many (satisfy isAlpha)
