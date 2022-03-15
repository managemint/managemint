{- app/Main.hs
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
{-# LANGUAGE ScopedTypeVariables #-}

import Scheduler
import Data.Text
import Yesod
import DatabaseUtil
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import Control.Concurrent.Async
import Data.Maybe

data AddRepository = AddRepository
        { repoURL :: Text
        , repoBranch :: Text
        }

addRepoForm = renderDivs $ AddRepository
        <$> areq textField "Repository URL" Nothing
        <*> areq textField "Branch" Nothing

newtype ButtonForm = ButtonForm
        { hiddenVal :: Int
        }

buttonForm val = renderDivs $ ButtonForm
        <$> areq hiddenField "" (Just val)

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

eventToStatus :: Entity Event -> String
eventToStatus (Entity eventid event) = "Success"

hostWidget :: Entity Run -> Int -> Int -> String -> ConnectionPool -> Widget
hostWidget (Entity runid run) playId taskId host pool = do
    event <- runSqlPool (selectFirst [EventPlay_id ==. playId, EventTask_id ==. taskId, EventHost ==. host, EventRunId ==. runid] []) pool
    toWidget
        [whamlet|
            <li>
                #{host}: #{eventToStatus (Data.Maybe.fromJust event)}
        |]

getHosts :: MonadIO m => Key Run -> Int -> Int -> ReaderT SqlBackend m [Single String]
getHosts run playId taskId = rawSql "SELECT DISTINCT event.host FROM event WHERE event.run_id=? AND event.play_id=? AND event.task_id=?" [toPersistValue run, toPersistValue playId,toPersistValue taskId]

taskWidget :: Entity Run -> Int -> Int -> ConnectionPool -> Widget
taskWidget entity@(Entity runid run) playId taskId pool = do
    event <- runSqlPool (selectFirst [EventPlay_id ==. playId, EventTask_id ==. taskId, EventRunId ==. runid] []) pool
    hosts <- runSqlPool (getHosts runid playId taskId) pool
    toWidget
        [whamlet|
            <li>
                #{eventTask (entityVal (Data.Maybe.fromJust event))}
                <ul>
                    $forall host <- hosts
                        ^{hostWidget entity playId taskId (unSingle host) pool}
        |]

getTaskIds :: MonadIO m => Key Run -> Int -> ReaderT SqlBackend m [Single Int]
getTaskIds run playId = rawSql "SELECT DISTINCT event.play_id FROM event WHERE event.run_id=? AND event.play_id=?" [toPersistValue run, toPersistValue playId]

playWidget :: Entity Run -> Int -> ConnectionPool -> Widget
playWidget entity@(Entity runid run) playId pool = do
    event <- runSqlPool (selectFirst [EventPlay_id ==. playId, EventRunId ==. runid] []) pool
    taskids <- runSqlPool (getTaskIds runid playId) pool
    toWidget
        [whamlet|
            <li>
                #{eventPlay (entityVal (Data.Maybe.fromJust event))}
                <ul>
                    $forall taskId <- taskids
                        ^{taskWidget entity playId (unSingle taskId) pool}
        |]

getPlayIds :: MonadIO m => Key Run -> ReaderT SqlBackend m [Single Int]
getPlayIds run = rawSql "SELECT DISTINCT event.play_id FROM event WHERE event.run_id=?" [toPersistValue run]

runWidget :: Entity Run -> ConnectionPool -> Widget
runWidget entity@(Entity runid run) pool = do
    playids <- runSqlPool (getPlayIds runid) pool
    toWidget
        [whamlet|
            <li>
                #{runTriggerdate run}
                <ul>
                    $forall playid <- playids
                        ^{playWidget entity (unSingle playid) pool}
        |]

playbookWidget :: Entity Playbook -> ConnectionPool -> Widget
playbookWidget (Entity playbookid playbook) pool = do
    runs <- runSqlPool (selectList [RunPlaybookId ==. playbookid] [Asc RunId]) pool
    toWidget
        [whamlet|
            <li>
                #{playbookPlaybookName playbook}
                <ul>
                    $forall entity <- runs
                        ^{runWidget entity pool}
        |]

projectWidget :: Entity Project -> ConnectionPool -> Widget
projectWidget (Entity projectid project) pool = do
    ((resultDeleteRepo, widgetDeleteRepo), enctype) <- runFormPost $ identifyForm (pack ("deleteRepo" ++ show (fromSqlKey projectid))) $ buttonForm (fromIntegral (fromSqlKey projectid))
    case resultDeleteRepo of
        FormSuccess (ButtonForm val) -> do
            runSqlPool (deleteWhere [ProjectId ==. toSqlKey (fromIntegral val)]) pool
            [whamlet||]
        _ -> case projectErrorMessage project of
                "" -> do
                    playbooks <- runSqlPool (selectList [PlaybookProjectId ==. projectid] [Asc PlaybookId]) pool
                    toWidget
                        [whamlet|
                            <li>
                                <ul>
                                    Project: #{projectUrl project} (#{projectBranch project})
                                    <form method=post action=@{HomeR}>
                                        ^{widgetDeleteRepo}
                                        <button>Remove
                                    $forall entity <- playbooks
                                        ^{playbookWidget entity pool}
                        |]
                err ->  [whamlet|
                            <font color="red">
                                #{err}
                        |]

getHomeR :: Handler Html
getHomeR = do
    ((resultAddRepo, widgetAddRepo), enctype) <- runFormPost $ identifyForm "addRepo" addRepoForm
    case resultAddRepo of
        FormSuccess (AddRepository repo branch) -> runDB ( insert $ Project (unpack repo) (unpack branch) "" "") >> pure ()
        _ -> pure ()
    projects <- runDB $ selectList [] [Asc ProjectId]
    App pool <- getYesod
    defaultLayout
        [whamlet|
            <ul>
                $forall entity <- projects
                    ^{projectWidget entity pool}
        <form method=post action=@{HomeR} enctype=#{enctype}>
            ^{widgetAddRepo}
            <button>Add
        |]

postHomeR :: Handler Html
postHomeR = getHomeR

runWebserver :: ConnectionPool -> IO ()
runWebserver conn = warp 3000 App { connections = conn }


connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo { connectHost     = "mdbtest-11.my.cum.re"
                                    , connectUser     = "hansible"
                                    , connectPassword = "AffqDbF2Vw5Aq7EHferw"
                                    , connectDatabase = "hansible"
                                    }

main :: IO ()
main = do
    runStderrLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ do
            runMigration migrateAll
        _ <- async $ schedule pool
        runWebserver pool
