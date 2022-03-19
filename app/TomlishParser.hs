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

module TomlishParser(compileTomlish, TomlishType(..), TomlishTree, Tree(..), TomlishKey(..), tomlish) where

import Parser

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Generics

-- TODO: comments, support more types as values
-- <Top>     ::= <Segment> \n <Top> | <Segment> | e
-- <Segment> ::= [ <Path> ] \n <Node> | [ <Path> ]
-- <Node>    ::= <Key> = <Value> \n <Node> | <Key> = <Value>
-- <Path>    ::= <Key>.<Path> | <Key>
-- <Key>     ::= Alphanumeric String (No Spaces)
-- <Value>   ::= Integer Value | <Quoted String>
-- <Quoted String> ::= " String ohne Quotes und Newline "

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
          <|> TomlishAntiString <$> (char '$' *> hsVarName)
          <|> TomlishAntiInt <$> (char '€' *> hsVarName)

parseKey :: Parser TomlishTree
parseKey =  (`Node` []) <$> parseTomlishKey

parseTomlishKey :: Parser TomlishKey
parseTomlishKey =  TomlishKey <$> alphaNum
               <|> TomlishAntiKey <$> (char '$' *> hsVarName)

parsePath :: Parser TomlishTree
parsePath =  parseKey
         <|> (\s t -> Node s [t]) <$> parseTomlishKey <* char '.' <*> parsePath

parseKeyValue :: Parser TomlishTree
parseKeyValue =  Leave <$> (parseTomlishKey <* skipSpaces <* char '=') <*> (skipSpaces *> parseValue)


parseNode :: Parser [TomlishTree]
parseNode =  (:[]) <$> parseKeyValue
         <|> (:) <$> (parseKeyValue <* skipNewline <* lineSeperator) <*> parseNode

parseSegmentTop :: Parser TomlishTree
parseSegmentTop =  char '[' *> skipSpaces *> parsePath <* skipSpaces <* char ']'

parseSegment :: Parser TomlishTree
parseSegment =  parseSegmentTop
            <|> addLeavesLinear <$> (parseSegmentTop <* skipNewline <* lineSeperator) <*> parseNode

parseTop :: Parser [TomlishTree]
parseTop =  (:[]) <$> parseSegment <* skipNewline
        <|> (:) <$> (parseSegment <* skipNewline <* lineSeperator) <*> parseTop

skipNewline :: Parser ()
skipNewline = () <$ many (char '\n' <|> space)

line :: Parser String
line = many $ notChars "\"\n"

lineSeperator :: Parser Char
lineSeperator = char '\n' <|> char ';'

--
-- tomlish quasi-quoter
--
--
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


tomlish :: QuasiQuoter
tomlish =  QuasiQuoter { quoteExp  = lift . compileTomlish
                       , quotePat  = \s -> do {tree <- foo s; dataToPatQ (const Nothing `extQ` antiTomlishKeyPat `extQ` antiTomlishTypePat) tree}
                       , quoteType = error "tomlish"
                       , quoteDec  = error "tomlish"
                       }

foo :: MonadFail m => String -> m [TomlishTree]
foo s = case compileTomlish s of
          Nothing -> fail "Failed to parse"
          Just v  -> return v

antiTomlishKeyPat :: TomlishKey -> Maybe PatQ
antiTomlishKeyPat (TomlishAntiKey v) = Just $ conP (mkName "TomlishKey") [varP (mkName v)]
antiTomlishKeyPat _ = Nothing

antiTomlishTypePat :: TomlishType -> Maybe PatQ
antiTomlishTypePat (TomlishAntiInt s) = Just $ conP (mkName "TomlishInt") [varP (mkName s)]
antiTomlishTypePat (TomlishAntiString s) = Just $ conP (mkName "TomlishString") [varP (mkName s)]
antiTomlishTypePat _ = Nothing
