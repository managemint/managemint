{- app/TomlishParser.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module TomlishParser where

import Parser
import Control.Applicative

-- TODO: comments, support more types as values
-- <Top>     ::= <Segment> \n <Top> | <Segment> | e
-- <Segment> ::= [ <Path> ] \n <Node> | [ <Path> ]
-- <Node>    ::= <Key> = <Value> \n <Node> | <Key> = <Value>
-- <Path>    ::= <Key>.<Path> | <Key>
-- <Key>     ::= Alphanumeric String (No Spaces)
-- <Value>   ::= Integer Value | <Quoted String> | [ <Values> ]
-- <Values>  ::= e | <Value>, <Values>
-- <Quoted String> ::= " String ohne Quotes und Newline "

data TomlishType= TomlishString String
                | TomlishInt Int
                | TomlishList [TomlishType]
                deriving (Show)

data Tree a b   = Node a [Tree a b]
                | Leave a b
                deriving (Show)

type TomlishTree = Tree String TomlishType

addLeavesLinear :: TomlishTree -> [TomlishTree] -> TomlishTree
addLeavesLinear (Node k []) b = Node k b
addLeavesLinear (Node k [t]) b = Node k [addLeavesLinear t b]
addLeavesLinear _ _ = error "addLeavesLinear undefined behaviour"

parseValue :: Parser TomlishType
parseValue = TomlishInt <$> integral
          <|> TomlishString <$> (char '"' *> line <* char '"')
          <|> TomlishString <$> (char '\'' *> line <* char '\'')
          <|> TomlishList [] <$ (char '[' <* skipSpaces <* char ']')
          <|> TomlishList <$> (char '[' *> parseList <* char ']')

parseList :: Parser [TomlishType]
parseList =  (:[]) <$> (skipSpaces *> parseValue <* skipSpaces)
         <|> (:)  <$> (skipSpaces *> parseValue <* skipSpaces <* char ',') <*> parseList

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
