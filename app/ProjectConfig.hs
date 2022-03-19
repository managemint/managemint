{-# LANGUAGE QuasiQuotes #-}
{- app/PlaybookConfiguration.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

--module ProjectConfig (PlaybookConfiguration (..), parseConfigFile, writePlaybookInDatabase) where

import DatabaseUtil
import ScheduleFormat
import TomlishParser
import Parser

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

foo :: Tomlishs -> Bool
foo [tomlish|hallo = $hi|] = hi == "hi"
foo _ = False

--parseConfigFile :: FilePath -> IO [PlaybookConfiguration]
--parseConfigFile path = do
--    contents <- readFile $ path ++ "/hansible.conf"
--    return $ case compileTomlish contents of
--               Left  err -> []
--               Right trees -> mapMaybe parseTomlishTree trees

-- [tomlish|[run.$name]\nfile = $f\nschedule = $s|]
--parseTomlishTree :: [TomlishTree] -> Maybe PlaybookConfiguration
--parseTomlishTree [tomlish|[run.$name];file = $f;schedule = $s|] =
--parseTomlishTree (Node (TomlishKey "run") [Node (TomlishKey name) [Leave (TomlishKey "file") (TomlishString f), Leave (TomlishKey "schedule") (TomlishString s)]]) =
--    case parseScheduleFormat s of
--      Nothing -> Nothing
--      Just s' -> Just PlaybookConfiguration{pName=name, pFile=f, pSchedule=s'}
--parseTomlishTree _ = Nothing

writePlaybookInDatabase :: Key Project -> PlaybookConfiguration -> ReaderT SqlBackend IO (Key Playbook)
writePlaybookInDatabase key p = do
    playbooks <- getPlaybooks key
    case filter ((==) (pName p) . playbookPlaybookName . entityVal) playbooks of
      []     -> insert $ Playbook key (pFile p) (pName p)
      (p':_) -> update (entityKey p') [PlaybookFile =. pFile p] >> return (entityKey p')
