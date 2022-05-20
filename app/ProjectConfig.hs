{- app/PlaybookConfiguration.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module ProjectConfig (PlaybookConfiguration (..), parseConfigFile, writePlaybookInDatabase) where

import DatabaseUtil
import ScheduleFormat
import Parser
import TomlishParser
import Tree (getLeavesAt, getValAt, Tree(Node, Leaf))
import Config

import Database.Persist.MySQL (runSqlPool, insert, update, entityKey, entityVal, (=.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (ask)
import Control.Exception (try)
import Data.Bifunctor (Bifunctor(first))

instance MonadFail (Either String) where
    fail = Left

data PlaybookConfiguration = PlaybookConfiguration
    { pName :: String
    , pFile :: String
    , pSchedule :: Schedule
    }
    deriving (Show)

-- | Tries to parse the config file in the folder pointed to by path
-- If the parsing fails updates the database
parseConfigFile :: FilePath -> IO (Either String [PlaybookConfiguration])
parseConfigFile path = do
    contents <- liftIO $ try $ readFile $ path ++ "/" ++ projectConfigFile
    return $ mapLeft (\(e :: IOError) -> show e) contents >>= parseTomlishTree >>= extractPlaybooks

extractPlaybooks :: TomlishTree -> Either String [PlaybookConfiguration]
extractPlaybooks tt = do
    trees <- getLeavesAt [TomlishRoot, TomlishKey "run"] tt
    mapM extractData trees

extractData :: TomlishTree -> Either String PlaybookConfiguration
extractData (Leaf _)    = Left "Error"
extractData tt@(Node name _) = do
    file <- extractString =<< getValAt [name, TomlishKey "file"] tt
    schedule <- (extractString =<< getValAt [name, TomlishKey "schedule"] tt)
            >>= funMaybeToRight "Failed to parse the schedule-format" parseScheduleFormat
    name' <- extractKeyString name
    return PlaybookConfiguration{pName=name', pFile=file, pSchedule=schedule}
        where
            extractString :: TomlishType -> Either String String
            extractString (TomlishString s) = Right s
            extractString _                 = Left "Not a TomlishString"
            extractKeyString :: TomlishKey -> Either String String
            extractKeyString (TomlishKey k) = Right k
            extractKeyString _              = Left "Not a TomlishKey"

writePlaybookInDatabase key p = do
    pool <- ask
    liftIO $ flip runSqlPool pool $ do
      playbooks <- getPlaybooks key
      case filter ((==) (pName p) . playbookPlaybookName . entityVal) playbooks of
        []     -> insert $ Playbook key (pFile p) (pName p)
        (p':_) -> update (entityKey p') [PlaybookFile =. pFile p] >> return (entityKey p')

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft = first

funMaybeToRight :: a -> (b -> Maybe c) -> b -> Either a c
funMaybeToRight d f x = maybeToRight d $ f x

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight l Nothing  = Left l
maybeToRight _ (Just v) = Right v
