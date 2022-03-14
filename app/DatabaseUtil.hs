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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    url String
    branch String
    errorMessage String
    deriving Show
Playbook
    projectId ProjectId
    playbookName String
    hosts String
Run
    playbookId PlaybookId
    status Int
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

getProjects :: ConnectionPool -> IO [Entity Project]
getProjects = runSqlPool (selectList [] [Asc ProjectId])

getPlaybooks :: Key Project -> ConnectionPool -> IO [Entity Playbook]
getPlaybooks projectid = runSqlPool (selectList [PlaybookProjectId ==. projectid] [Asc PlaybookId])

addRun :: Run -> ConnectionPool -> IO (Key Run)
addRun run = runSqlPool (insert run)

addEvent :: Event -> ConnectionPool -> IO (Key Event)
addEvent event = runSqlPool (insert event)

newtype App = App { connections :: ConnectionPool }

