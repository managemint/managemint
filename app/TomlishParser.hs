{-# LANGUAGE DeriveDataTypeable #-}
{- app/TomlishParser.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

--module TomlishParser(TomlishType(..), Tomlishs, Tomlish(..), TomlishKey(..), tomlish) where
module TomlishParser where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Generics
import Data.Char (isLower, isAlpha, digitToInt)
import Text.ParserCombinators.Parsec

data TomlishKey = TomlishKey String
                | TomlishAntiKey String
                deriving (Show, Data)

data TomlishType = TomlishString String
                 | TomlishAntiString String
                 | TomlishInt Int
                 | TomlishAntiInt String
                 deriving (Show, Data)

data Tomlish = Key TomlishKey | KeyVals (TomlishKey, TomlishType)
    deriving (Show,Data)

type Tomlishs = [Tomlish]


-- /PARSER/ --

-- TODO: comments, support more types as values
-- <Top>     ::= <Node> | <Node> \n <Top>
-- <Node>    ::= [ <Path> ] | <KeyVal>
-- <KeyVal>  ::= <Key> = <Value> \n <Node> | <Key> = <Value>
-- <Path>    ::= <Key>.<Path> | <Key>
-- <Key>     ::= Alphanumeric String (No Spaces)
-- <Value>   ::= Integer Value | <Quoted String>
-- <Quoted String> ::= " String ohne Double Quotes und Newline " | ' String ohne Single Quotes und Newline '

parseTomlish :: MonadFail m => (String, Int, Int) -> String -> m Tomlishs
parseTomlish (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do pos <- getPosition
           setPosition $ flip setSourceName file $ flip setSourceLine line $ setSourceColumn pos col
           spaces
           e <- parseTop
           eof
           return e

parseTop :: CharParser st Tomlishs
parseTop = parseNode `chainl1` (skipMany1 newline >> return (++))

parseNode :: CharParser st Tomlishs
parseNode =  brackets parsePath
         <|> (:[]) <$> parseKeyValue

parseKeyValue :: CharParser st Tomlish
parseKeyValue = curry KeyVals <$> (parseTomlishKey <* spaces <* char '=') <*> (spaces *> parseValue)

parseValue :: CharParser st TomlishType
parseValue =  TomlishInt <$> integral
          <|> TomlishString <$> between (char '"') (char '"') (many1 (noneOf ['\n','"']))
          <|> TomlishString <$> between (char '\'') (char '\'') (many1 (noneOf ['\n','\'']))
          <|> TomlishAntiString <$> (char '$' *> hsVarName)
          <|> TomlishAntiInt <$> (char '€' *> hsVarName)

parsePath :: CharParser st Tomlishs
parsePath = fmap ((:[]) . Key) parseTomlishKey `chainl1` (char '.' >> return (++))

parseTomlishKey :: CharParser st TomlishKey
parseTomlishKey =  TomlishKey <$> many1 alphaNum
               <|> TomlishAntiKey <$> (char '#' *> hsVarName)

brackets :: CharParser st a -> CharParser st a
brackets = between (char '[' *> spaces) (spaces <* char ']')

skipNewline :: CharParser st ()
skipNewline = skipMany $ char '\n'

hsVarName :: CharParser st String
hsVarName = (:) <$> satisfy isLower <*> many (satisfy isAlpha)

integral :: CharParser st Int
integral =  natural
        <|> negate <$> (char '-' *> natural)

natural :: CharParser st Int
natural = accum <$> many1 (digitToInt <$> digit)
    where accum = foldl (\d n-> n + d*10) 0

-- \PARSER\ --


-- /QUASI-QUOTER/ --

tomlish :: QuasiQuoter
tomlish =  QuasiQuoter { quoteExp  = quoteTomlishTreeExp
                       , quotePat  = quoteTomlishTreePat
                       , quoteType = error "tomlish"
                       , quoteDec  = error "tomlish"
                       }

quoteTomlishTreeExp :: String -> ExpQ
quoteTomlishTreeExp s = do loc <- location
                           let pos = ( loc_filename loc
                                     , fst (loc_start loc)
                                     , snd (loc_start loc))
                           tomli <- parseTomlish pos s
                           dataToExpQ (const Nothing `extQ` antiTomlishTypeExp) tomli

antiTomlishTypeExp :: TomlishType -> Maybe ExpQ
antiTomlishTypeExp (TomlishAntiString s) = Just $ appE (conE (mkName "TomlishString")) (varE (mkName s))
antiTomlishTypeExp (TomlishAntiInt s)    = Just $ appE (conE (mkName "TomlishInt"))    (varE (mkName s))
antiTomlishTypeExp _ = Nothing

quoteTomlishTreePat :: String -> PatQ
quoteTomlishTreePat s = do loc <- location
                           let pos = ( loc_filename loc
                                     , fst (loc_start loc)
                                     , snd (loc_start loc))
                           tomli <- parseTomlish pos s
                           dataToPatQ (const Nothing `extQ` antiTomlishTypePat) tomli

antiTomlishTypePat :: TomlishType -> Maybe PatQ
antiTomlishTypePat (TomlishAntiString s) = Just $ conP (mkName "TomlishString") [varP (mkName s)]
antiTomlishTypePat (TomlishAntiInt s)    = Just $ conP (mkName "TomlishInt")    [varP (mkName s)]
antiTomlishTypePat _ = Nothing

-- \QUASI-QUOTER\ --
