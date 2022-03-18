{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{- app/TomlishParser.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module TomlishParser(compileTomlish, TomlishType(..), TomlishTree, Tree(..), tomlish) where

import Parser

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- TODO: comments, support more types as values
-- <Top>     ::= <Segment> \n <Top> | <Segment> | e
-- <Segment> ::= [ <Path> ] \n <Node> | [ <Path> ]
-- <Node>    ::= <Key> = <Value> \n <Node> | <Key> = <Value>
-- <Path>    ::= <Key>.<Path> | <Key>
-- <Key>     ::= Alphanumeric String (No Spaces)
-- <Value>   ::= Integer Value | <Quoted String>
-- <Quoted String> ::= " String ohne Quotes und Newline "

data TomlishType= TomlishString String
                | TomlishInt Int
                deriving (Show)

data Tree a b   = Node a [Tree a b]
                | Leave a b
                deriving (Show)

type TomlishTree = Tree String TomlishType

compileTomlish :: String -> Maybe [TomlishTree]
compileTomlish = parse parseTop

addLeavesLinear :: TomlishTree -> [TomlishTree] -> TomlishTree
addLeavesLinear (Node k []) b = Node k b
addLeavesLinear (Node k [t]) b = Node k [addLeavesLinear t b]
addLeavesLinear _ _ = error "addLeavesLinear undefined behaviour"

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

skipNewline :: Parser ()
skipNewline = () <$ many (char '\n' <|> space)

line :: Parser String
line = many $ notChars "\"\n"

--
-- tomlish quasi-quoter
--

instance Lift TomlishType where
    lift (TomlishString s) = appE (conE 'TomlishString) (lift s)
    lift (TomlishInt i) = appE (conE 'TomlishInt) (lift i)
    liftTyped = error "TomlishType liftTyped"

instance (Lift a, Lift b) => Lift (Tree a b) where
    lift (Node k v) = appE (appE (conE 'Node) (lift k)) (lift v)
    lift (Leave k v) = appE (appE (conE 'Leave) (lift k)) (lift v)
    liftTyped = error "Tree liftTyped"


tomlish :: QuasiQuoter
tomlish =  QuasiQuoter { quoteExp  = lift .  compileTomlish
                       , quotePat  = error "tomlish"
                       , quoteType = error "tomlish"
                       , quoteDec  = error "tomlish"
                       }
