{- app/DatabaseUtil.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module DatabaseUtil where

import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import Control.Monad.Trans.Reader
import Yesod.Static

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    url String
    branch String
    errorMessage String
    oid String
    deriving Show
Playbook
    projectId ProjectId
    file String
    playbookName String
Run
    playbookId PlaybookId
    status Int
    oid String
    triggerdate String
Event
    task String
    task_id Int
    play String
    play_id Int
    host String
    runId RunId
    is_changed Bool
    is_skipped Bool
    is_failed Bool
    is_unreachable Bool
    is_item Bool
    item String
    output String
JobQueue
    playbookId PlaybookId
    arguments String
    triggerDate String
|]

getProjects :: ReaderT SqlBackend IO [Entity Project]
getProjects = selectList [] [Asc ProjectId]

getPlaybooks :: Key Project -> ReaderT SqlBackend IO [Entity Playbook]
getPlaybooks projectid = selectList [PlaybookProjectId ==. projectid] [Asc PlaybookId]

getJobPlaybook :: Key Playbook -> ReaderT SqlBackend IO (Entity Playbook)
getJobPlaybook playId = head <$> selectList [PlaybookId ==. playId] []

getPlaybookProject :: Key Project -> ReaderT SqlBackend IO (Entity Project)
getPlaybookProject proId = head <$> selectList [ProjectId ==. proId] []

addRun :: Run -> ConnectionPool -> IO (Key Run)
addRun run = runSqlPool (insert run)

addEvent :: Event -> ConnectionPool -> IO (Key Event)
addEvent event = runSqlPool (insert event)

data Hansible = Hansible{ connections :: ConnectionPool, getStatic :: Static }

