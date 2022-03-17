{- app/PlaybookConfiguration.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module ProjectConfig (PlaybookConfiguration (..), parseConfigFile, writePlaybookInDatabase) where

import DatabaseUtil
import ScheduleFormat
import TomlishParser

import qualified Data.Map as M
import Database.Persist.MySQL hiding (get)
import Control.Monad.Trans.Reader
import System.IO
import Data.Maybe (mapMaybe)
import Control.Monad.IO.Class (liftIO)

data PlaybookConfiguration = PlaybookConfiguration
    { pName :: String
    , pFile :: String
    , pSchedule :: Schedule
    }
    deriving (Show)

parseConfigFile :: FilePath -> IO [PlaybookConfiguration]
parseConfigFile path = do
    contents <- readFile $ path ++ "/hansible.conf"
    return $ case parse parseTop contents of
               Nothing    -> []
               Just trees -> mapMaybe parseTomlishTree trees

parseTomlishTree :: TomlishTree -> Maybe PlaybookConfiguration
parseTomlishTree (Node "run" [Node name [Leave "file" (TomlishString f), Leave "schedule" (TomlishString s)]]) =
    case parseScheduleFormat s of
      Nothing -> Nothing
      Just s' -> Just PlaybookConfiguration{pName=name, pFile=f, pSchedule=s'}
parseTomlishTree _ = Nothing

writePlaybookInDatabase :: Key Project -> PlaybookConfiguration -> ReaderT SqlBackend IO (Key Playbook)
writePlaybookInDatabase key p = do
    playbooks <- getPlaybooks key
    case filter ((==) (pName p) . playbookPlaybookName . entityVal) playbooks of
      []     -> insert $ Playbook key (pFile p) (pName p)
      (p':_) -> update (entityKey p') [PlaybookFile =. pFile p] >> return (entityKey p')
