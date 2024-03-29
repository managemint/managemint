{-# LANGUAGE OverloadedStrings #-}
{- app/Config.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module Config where
import Control.Monad.Logger (LogLevel(..))
import Data.Text (Text)

-- Format: <module><Name>

loggerUtilLogSourcesBlocklist = [ "SQL" ] :: [Text]
loggerUtilLogLevel = LevelDebug

sockReadLen = 2048 :: Int

schedulerFailMax = 3 :: Int
schedulerUserTemplateKey = "USERTEMPLATE"
schedulerFolders = ["app",".stack-work","venv","csrc",".git","static"] -- TODO: Remove this as soon as the repos live in their own folder
schedulerRepoRoot = "." :: FilePath

projectConfigFile = "managemint.conf"

executorSockPath = "/tmp/managemint3.sock"

webserverPort = 3002 :: Int

mainConfigFile = "./managemint.conf"
