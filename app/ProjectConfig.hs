{- app/PlaybookConfiguration.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module ProjectConfig (PlaybookConfiguration (..), parseConfigFile, writePlaybookInDatabase) where

import DatabaseUtil
import ScheduleFormat
import Parser
import TomlishParser
import Tree (getLeavesAt, Tree(Node))

import Database.Persist.MySQL (runSqlPool, insert, update, entityKey, entityVal, (=.))
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (ask)
import Control.Exception (try)

data PlaybookConfiguration = PlaybookConfiguration
    { pName :: String
    , pFile :: String
    , pSchedule :: Schedule
    }
    deriving (Show)

-- | Tries to parse the config file in the folder pointed to by path
-- If the parsing fails updates the database
parseConfigFile :: FilePath -> IO [PlaybookConfiguration]
parseConfigFile path = do
    contents <- liftIO $ try $ readFile $ path ++ "/hansible.conf"
    let contents' = (\case {Left (e :: IOError) -> Nothing; Right v -> Just v})
                      contents >>= parseTomlishTree >>= extractPlaybooks
    return $ fromMaybe [] contents'

-- TODO: This is ugly
extractPlaybooks :: TomlishTree -> Maybe [PlaybookConfiguration]
extractPlaybooks tt = do
    trees <- getLeavesAt [TomlishRoot, TomlishKey "run"] tt
    mapM (extractData . (\t -> Node TomlishRoot [Node (TomlishKey "run") [t]])) trees

extractData :: TomlishTree -> Maybe PlaybookConfiguration
extractData [tomlish|[run.$name];file = $f;schedule = $s|] =
    case parseScheduleFormat s of
      Nothing -> Nothing
      Just s' -> Just PlaybookConfiguration{pName=name, pFile=f, pSchedule=s'}
extractData _ = Nothing

writePlaybookInDatabase key p = do
    pool <- ask
    liftIO $ flip runSqlPool pool $ do
      playbooks <- getPlaybooks key
      case filter ((==) (pName p) . playbookPlaybookName . entityVal) playbooks of
        []     -> insert $ Playbook key (pFile p) (pName p)
        (p':_) -> update (entityKey p') [PlaybookFile =. pFile p] >> return (entityKey p')
