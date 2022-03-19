{-# LANGUAGE QuasiQuotes, TemplateHaskell, DeriveDataTypeable #-}
{- app/TomlishParser.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module TomlishParser(compileTomlish, tomlish, TomlishTree, TomlishKey(TomlishKey), TomlishType(TomlishString, TomlishInt)) where

import Parser

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Generics

data TomlishKey = TomlishKey String
                | TomlishAntiKey String
                deriving (Show, Data)

data TomlishType= TomlishString String
                | TomlishAntiString String
                | TomlishInt Int
                | TomlishAntiInt String
                deriving (Show, Data)

data Tree a b   = Node a [Tree a b]
                | Leave a b
                deriving (Show, Data)

type TomlishTree = Tree TomlishKey TomlishType

instance Lift TomlishKey where
    lift (TomlishKey s) = appE (conE 'TomlishKey) (lift s)
    lift (TomlishAntiKey s) = appE (conE 'TomlishKey) (unboundVarE (mkName s))
    liftTyped = error "TomlishKey liftTyped"

instance Lift TomlishType where
    lift (TomlishString s) = appE (conE 'TomlishString) (lift s)
    lift (TomlishAntiString s) = appE (conE 'TomlishString) (unboundVarE (mkName s))
    lift (TomlishInt i) = appE (conE 'TomlishInt) (lift i)
    lift (TomlishAntiInt s) = appE (conE 'TomlishInt) (unboundVarE (mkName s))
    liftTyped = error "TomlishType liftTyped"

instance (Lift a, Lift b) => Lift (Tree a b) where
    lift (Node k v) = appE (appE (conE 'Node) (lift k)) (lift v)
    lift (Leave k v) = appE (appE (conE 'Leave) (lift k)) (lift v)
    liftTyped = error "Tree liftTyped"


-- /PARSER/ --

-- TODO: comments, support more types as values
-- <Top>     ::= <Segment> \n <Top> | <Segment> | e
-- <Segment> ::= [ <Path> ] \n <Node> | [ <Path> ]
-- <Node>    ::= <Key> = <Value> \n <Node> | <Key> = <Value>
-- <Path>    ::= <Key>.<Path> | <Key>
-- <Key>     ::= Alphanumeric String (No Spaces)
-- <Value>   ::= Integer Value | <Quoted String> | [ <Values> ]
-- <Values>  ::= e | <Value>, <Values>
-- <Quoted String> ::= " String ohne Quotes und Newline "

failParse :: MonadFail m => String -> m [TomlishTree]
failParse s = case compileTomlish s of
                Nothing -> fail "Failed to parse"
                Just v  -> return v

compileTomlish :: String -> Maybe [TomlishTree]
compileTomlish = parse parseTop

compile :: String -> [TomlishTree]
compile s = case compileTomlish s of
              Nothing -> error "parse error"
              Just v  -> v

addLeavesLinear :: TomlishTree -> [TomlishTree] -> TomlishTree
addLeavesLinear (Node k []) b = Node k b
addLeavesLinear (Node k [t]) b = Node k [addLeavesLinear t b]
addLeavesLinear _ _ = error "addLeavesLinear undefined behaviour"

parseTop :: Parser [TomlishTree]
parseTop =  (:[]) <$> parseSegment <* skipNewline
        <|> (:) <$> (parseSegment <* skipNewline <* lineSeperator) <*> parseTop

parseSegmentTop :: Parser TomlishTree
parseSegmentTop =  char '[' *> skipSpaces *> parsePath <* skipSpaces <* char ']'

parseSegment :: Parser TomlishTree
parseSegment =  parseSegmentTop
            <|> addLeavesLinear <$> (parseSegmentTop <* skipNewline <* lineSeperator) <*> parseNode

parseNode :: Parser [TomlishTree]
parseNode =  (:[]) <$> parseKeyValue
         <|> (:) <$> (parseKeyValue <* skipNewline <* lineSeperator) <*> parseNode

parsePath :: Parser TomlishTree
parsePath =  parseKey
         <|> (\s t -> Node s [t]) <$> parseTomlishKey <* char '.' <*> parsePath

parseKey :: Parser TomlishTree
parseKey =  (`Node` []) <$> parseTomlishKey

parseKeyValue :: Parser TomlishTree
parseKeyValue =  Leave <$> (parseTomlishKey <* skipSpaces <* char '=') <*> (skipSpaces *> parseValue)

parseTomlishKey :: Parser TomlishKey
parseTomlishKey =  TomlishKey <$> alphaNum
               <|> TomlishAntiKey <$> (char '$' *> hsVarName)

parseValue :: Parser TomlishType
parseValue = TomlishInt <$> integral
          <|> TomlishString <$> (char '"' *> line '"' <* char '"')
          <|> TomlishString <$> (char '\'' *> line '\''<* char '\'')
          <|> TomlishAntiString <$> (char '$' *> hsVarName)
          <|> TomlishAntiInt <$> (char '€' *> hsVarName)

skipNewline :: Parser ()
skipNewline = () <$ many (char '\n' <|> space)

line :: Char -> Parser String
line c = many $ notChars (c:"\n;")

lineSeperator :: Parser Char
lineSeperator = char '\n' <|> char ';'

-- \PARSER\ --


-- /QUASI-QUOTER/-

tomlish :: QuasiQuoter
tomlish =  QuasiQuoter { quoteExp  = lift . compile
                       , quotePat  = \s -> do {tree <- failParse s; dataToPatQ (const Nothing `extQ` antiTomlishKeyPat `extQ` antiTomlishTypePat) tree}
                       , quoteType = error "tomlish"
                       , quoteDec  = error "tomlish"
                       }

-- This is based on exercise sheet 11 and the example to QuasiQuoters given on the Haskell wiki https://wiki.haskell.org/Quasiquotation

antiTomlishKeyPat :: TomlishKey -> Maybe PatQ
antiTomlishKeyPat (TomlishAntiKey v) = Just $ conP (mkName "TomlishKey") [varP (mkName v)]
antiTomlishKeyPat _ = Nothing

antiTomlishTypePat :: TomlishType -> Maybe PatQ
antiTomlishTypePat (TomlishAntiInt s) = Just $ conP (mkName "TomlishInt") [varP (mkName s)]
antiTomlishTypePat (TomlishAntiString s) = Just $ conP (mkName "TomlishString") [varP (mkName s)]
antiTomlishTypePat _ = Nothing

-- \QUASI-QUOTER\ --
