{- app/TomlishParser.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

module TomlishParser(TomlishTree, TomlishType(TomlishString, TomlishInt), TomlishKey(TomlishRoot, TomlishKey), tomlish, parseTomlishTree) where

import Tree
import Language.Haskell.TH
    ( PatQ
    , ExpQ
    , location
    , Loc (loc_filename, loc_start)
    , appE
    , conE
    , mkName
    , varE
    , conP
    , varP)
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToPatQ, dataToExpQ)
import Data.Generics (Data, extQ)
import Data.Char (isLower, isAlpha, digitToInt)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Foldable (fold)
import Text.ParserCombinators.Parsec
    ( CharParser
    , runParser
    , getPosition
    , setPosition
    , setSourceName
    , setSourceLine
    , setSourceColumn
    , spaces
    , eof
    , skipMany1
    , newline
    , (<|>)
    , char
    , between
    , many1
    , noneOf
    , alphaNum
    , skipMany
    , satisfy
    , many
    , digit
    , space
    , sepEndBy, sepBy1)
import Control.Lens ((&), (%~), _1, (<&>))


data TomlishKey = TomlishKey String
                | TomlishAntiKey String
                | TomlishRoot
                deriving (Show, Data)

data TomlishType = TomlishString String
                 | TomlishAntiString String
                 | TomlishInt Int
                 | TomlishAntiInt String
                 deriving (Show, Data, Eq)

data Tomlish = Key TomlishKey | KeyVal TomlishKey TomlishType | NewSegment
    deriving (Show,Data,Eq)

type TomlishTree = Tree TomlishKey TomlishType

type TomlishClub = ([TomlishKey],[(TomlishKey,TomlishType)])

instance Eq TomlishKey where
    (==) (TomlishKey s) (TomlishKey s') = s == s'
    (==)  TomlishRoot    TomlishRoot    = True
    (==)  _              _              = False


-- /TREE-BUILDING/ --

buildTomlishTree :: MonadFail m => [Tomlish] -> m TomlishTree
buildTomlishTree ts = do
    clubs <- classifyClubs <$> createClubs ts
    leaves <- fold <$> mapM createSubtrees clubs
    return $ Node TomlishRoot leaves

createSubtrees :: MonadFail m => [TomlishClub] -> m [TomlishTree]
createSubtrees tcs
    | length tcs == 1 = return $ clubToTree $ head tcs
    | otherwise       = do
        key <- clubHead $ head tcs
        leaves <- fold <$> mapM createSubtrees (classifyClubs $ map clubTail tcs)
        return [Node key leaves]

clubHead :: MonadFail m => TomlishClub -> m TomlishKey
clubHead = safeHead . fst

clubTail :: TomlishClub -> TomlishClub
clubTail c = c & _1 %~ drop 1

clubToTree :: TomlishClub -> [TomlishTree]
clubToTree ([],kv)   = map (\(k,v) -> Node k [Leaf v]) kv
clubToTree (x:xs,kv) = [Node x $ clubToTree (xs,kv)]

-- | Extracts the segments from a tomlish-list.
-- A segment ist a list of keys (the path) with a list of key values (the leaves).
-- It forms a list of Keys with a List of key-values at the end, thefore a club
createClubs :: MonadFail m => [Tomlish] -> m [TomlishClub]
createClubs = mapM createClub . seperateOn NewSegment

createClub :: MonadFail m => [Tomlish] -> m TomlishClub
createClub ts = mapM extractKeyVal vals <&> (map (\(Key k) -> k) keys,)
    where
        (keys,vals) = span isKey ts

extractKeyVal :: MonadFail m => Tomlish -> m (TomlishKey, TomlishType)
extractKeyVal (KeyVal k v) = return (k,v)
extractKeyVal _ = fail "Not a key-value"

-- | Classifies 'TomlishClub's by their first key.
-- Key-values always go in their own sperate class
classifyClubs :: [TomlishClub] -> [[TomlishClub]]
classifyClubs = classifyBy equiv
    where equiv tc tc' = fromMaybe False ((==) <$> firstKey tc <*> firstKey tc')
          firstKey (x:_,_) = Just x
          firstKey _       = Nothing

isKeyVal :: Tomlish -> Bool
isKeyVal (KeyVal _ _) = True
isKeyVal _            = False

isKey :: Tomlish -> Bool
isKey (Key _) = True
isKey _       = False

-- \TREE-BUILDING\ --


-- /PARSER/ --

-- TODO: comments, support more types as values
-- <Top>     ::= <Node> | <Node> \n <Top> | <Node> ; <Top>
-- <Node>    ::= [ <Path> ] | <Key> = <Value>
-- <Path>    ::= <Key>.<Path> | <Key>
-- <Key>     ::= Alphanumeric String (No Spaces)
-- <Value>   ::= Integer Value | <Quoted String>
-- <Quoted String> ::= " String without Double Quotes or Newline " | ' String without Single Quotes or Newline '

parseTomlishTree :: MonadFail m => String -> m TomlishTree
parseTomlishTree str =
    -- TODO: Remove is only quickfix
    case runParser parseTop () "" (map (\c -> if c == '\n' then ';' else c) str) of
      Left err -> fail $ show err
      Right t  -> buildTomlishTree t

parseTomlish :: MonadFail m => (String, Int, Int) -> String -> m [Tomlish]
parseTomlish (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do pos <- getPosition
           setPosition $ (`setSourceName` file) $ (`setSourceLine` line) $ setSourceColumn pos col
           spaces
           e <- parseTop
           eof
           return e

parseTop :: CharParser st [Tomlish]
parseTop = concat <$> sepEndBy parseNode (skipMany1 linebreak)

linebreak :: CharParser st ()
linebreak = spaces *> (newline <|> char ';') *> spaces

parseNode :: CharParser st [Tomlish]
parseNode =  (NewSegment:) <$> brackets parsePath
         <|> (:[]) <$> parseKeyValue

parseKeyValue :: CharParser st Tomlish
parseKeyValue = KeyVal <$> (parseTomlishKey <* spaces <* char '=') <*> (spaces *> parseValue)

parseValue :: CharParser st TomlishType
parseValue =  TomlishInt <$> integral
          <|> TomlishString <$> between (char '"') (char '"') (many1 (noneOf [';','\n','"']))
          <|> TomlishString <$> between (char '\'') (char '\'') (many1 (noneOf [';','\n','\'']))
          <|> TomlishAntiString <$> (char '$' *> hsVarName)
          <|> TomlishAntiInt <$> (char '€' *> hsVarName)

parsePath :: CharParser st [Tomlish]
parsePath = map Key <$> sepBy1 parseTomlishKey (char '.')

parseTomlishKey :: CharParser st TomlishKey
parseTomlishKey =  TomlishKey <$> many1 alphaNum
               <|> TomlishAntiKey <$> (char '$' *> hsVarName)

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


-- /QUASI-QUOTER/-

tomlish :: QuasiQuoter
tomlish =  QuasiQuoter { quoteExp  = quoteTomlishTreeExp
                       , quotePat  = quoteTomlishTreePat
                       , quoteType = const $ fail "Quasi-quoter for types not implemented"
                       , quoteDec  = const $ fail "Quasi-quoter for declarations not implemented"
                       }

quoteTomlishTreeExp :: String -> ExpQ
quoteTomlishTreeExp s = do loc <- location
                           let pos = ( loc_filename loc
                                     , fst (loc_start loc)
                                     , snd (loc_start loc))
                           tomli <- buildTomlishTree =<< parseTomlish pos s
                           dataToExpQ (const Nothing `extQ` antiTomlishTypeExp `extQ` antiTomlishKeyExp) tomli

antiTomlishTypeExp :: TomlishType -> Maybe ExpQ
antiTomlishTypeExp (TomlishAntiString s) = Just $ appE (conE (mkName "TomlishString")) (varE (mkName s))
antiTomlishTypeExp (TomlishAntiInt s)    = Just $ appE (conE (mkName "TomlishInt"))    (varE (mkName s))
antiTomlishTypeExp _ = Nothing

antiTomlishKeyExp :: TomlishKey -> Maybe ExpQ
antiTomlishKeyExp (TomlishAntiKey s) = Just $ appE (conE (mkName "TomlishKey")) (varE (mkName s))
antiTomlishKeyExp _ = Nothing

quoteTomlishTreePat :: String -> PatQ
quoteTomlishTreePat s = do loc <- location
                           let pos = ( loc_filename loc
                                     , fst (loc_start loc)
                                     , snd (loc_start loc))
                           tomli <- buildTomlishTree =<< parseTomlish pos s
                           dataToPatQ (const Nothing `extQ` antiTomlishTypePat `extQ` antiTomlishKeyPat) tomli

antiTomlishTypePat :: TomlishType -> Maybe PatQ
antiTomlishTypePat (TomlishAntiString s) = Just $ conP (mkName "TomlishString") [varP (mkName s)]
antiTomlishTypePat (TomlishAntiInt s)    = Just $ conP (mkName "TomlishInt")    [varP (mkName s)]
antiTomlishTypePat _ = Nothing

antiTomlishKeyPat :: TomlishKey -> Maybe PatQ
antiTomlishKeyPat (TomlishAntiKey s) = Just $ conP (mkName "TomlishKey") [varP (mkName s)]
antiTomlishKeyPat _ = Nothing

-- \QUASI-QUOTER\ --


-- /EXTRA/ --

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy f []     = []
classifyBy f (x:xs) = (x : filter (f x) xs)
                    : classifyBy f (filter (not . f x) xs)
-- | Decompose a list into its head and tail.
--
-- * If the list is empty, returns @('mempty', [])@
-- * If the list is non-empty, returns @(x, xs)@,
-- where @x@ is the head of the list and @xs@ its tail.
uncons :: Monoid a => [a] -> (a,[a])
uncons []     = (mempty,[])
uncons (x:xs) = (x,xs)

seperateOn :: Eq a => a -> [a] -> [[a]]
seperateOn _ [] = []
seperateOn sep list = l : seperateOn sep (drop 1 r)
    where (l,r) = span (/= sep) list

safeHead :: MonadFail m => [a] -> m a
safeHead []    = fail "Head on empty list"
safeHead (x:_) = return x

-- \EXTRA\ --
