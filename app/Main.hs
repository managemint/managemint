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
import Control.Monad.State
import Control.Concurrent.Async
import Data.Maybe
import Data.Foldable

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

mkYesod "Hansible" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod Hansible

instance RenderMessage Hansible FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist Hansible where
    type YesodPersistBackend Hansible = SqlBackend

    runDB action = do
        Hansible pool <- getYesod
        runSqlPool action pool

data Status = Ok | Failed | Running

generateStatusIndicator :: Status -> Widget
generateStatusIndicator success =
    let s = case success of
            Ok -> "green"
            Failed -> "red"
            Running -> "blue" ::String in
    toWidget
        [whamlet|
            <font color=#{s}>
                ●
        |]
joinStatus :: Status -> Status -> Status
joinStatus Ok Ok = Ok
joinStatus _ _ = Failed

eventToStatus :: Entity Event -> (String, Status)
eventToStatus (Entity eventid event)
  | eventIs_changed event = ("CHANGED", Ok)
  | eventIs_failed event = ("FAILED", Failed)
  | eventIs_skipped event = ("SKIPPED", Ok)
  | eventIs_unreachable event = ("UNREACHABLE", Failed)
  | otherwise = ("SUCCESS", Ok)

hostWidget :: Entity Run -> Int -> Int -> String -> ConnectionPool -> IO (Widget, Status)
hostWidget (Entity runid run) playId taskId host pool = do
    event <- runSqlPool (selectFirst [EventPlay_id ==. playId, EventTask_id ==. taskId, EventHost ==. host, EventRunId ==. runid] []) pool
    let (text, status) = eventToStatus (Data.Maybe.fromJust event)
    return (toWidget
        [whamlet|
            <li>
                #{host}: #{text}
        |], status)

getHosts :: MonadIO m => Key Run -> Int -> Int -> ReaderT SqlBackend m [Single String]
getHosts run playId taskId = rawSql "SELECT DISTINCT event.host FROM event WHERE event.run_id=? AND event.play_id=? AND event.task_id=?" [toPersistValue run, toPersistValue playId,toPersistValue taskId]

taskWidget :: Entity Run -> Int -> Int -> ConnectionPool -> IO (Widget, Status)
taskWidget entity@(Entity runid run) playId taskId pool = do
    event <- runSqlPool (selectFirst [EventPlay_id ==. playId, EventTask_id ==. taskId, EventRunId ==. runid] []) pool
    hostNames <- runSqlPool (getHosts runid playId taskId) pool
    hosts <- mapM (\x -> hostWidget entity playId taskId (unSingle x) pool) hostNames
    let (hostWidgets, status) = Data.Foldable.foldl (\(ws, ss) (w, s) -> (w:ws, joinStatus s ss)) ([], Ok) hosts
    return (toWidget
        [whamlet|
            <li>
                #{eventTask (entityVal (Data.Maybe.fromJust event))} ^{generateStatusIndicator status}
                <ul class="hosts">
                    $forall host <- hostWidgets
                        ^{host}
        |], status)

getTaskIds :: MonadIO m => Key Run -> Int -> ReaderT SqlBackend m [Single Int]
getTaskIds run playId = rawSql "SELECT DISTINCT event.task_id FROM event WHERE event.run_id=? AND event.play_id=?" [toPersistValue run, toPersistValue playId]

playWidget :: Entity Run -> Int -> ConnectionPool -> IO (Widget, Status)
playWidget entity@(Entity runid run) playId pool = do
    event <- runSqlPool (selectFirst [EventPlay_id ==. playId, EventRunId ==. runid] []) pool
    taskids <- runSqlPool (getTaskIds runid playId) pool
    tasks <- mapM (\x -> taskWidget entity playId (unSingle x) pool) taskids
    let (taskWidgets, status) = Data.Foldable.foldl (\(ws, ss) (w, s) -> (w:ws, joinStatus s ss)) ([], Ok) tasks
    return (toWidget
        [whamlet|
            <li>
                #{eventPlay (entityVal (Data.Maybe.fromJust event))} ^{generateStatusIndicator status}
                <ul class="tasks">
                    $forall task <- taskWidgets
                        ^{task}
        |], status)

getPlayIds :: MonadIO m => Key Run -> ReaderT SqlBackend m [Single Int]
getPlayIds run = rawSql "SELECT DISTINCT event.play_id FROM event WHERE event.run_id=?" [toPersistValue run]

runWidget :: Entity Run -> ConnectionPool -> Widget
runWidget entity@(Entity runid run) pool = do
    playids <- runSqlPool (getPlayIds runid) pool
    plays <- liftIO $ mapM (\x -> playWidget entity (unSingle x) pool) playids
    let (playWidgets, _treeStatus) = Data.Foldable.foldl (\(ws, ss) (w, s) -> (w:ws, joinStatus s ss)) ([], Ok) plays
    let status = case runStatus run of
                    -1 -> Failed
                    0 -> Ok
                    1 -> Running
                    _ -> Failed
    toWidget
        [whamlet|
            <li>
                #{runTriggerdate run} ^{generateStatusIndicator status}
                <ul class="plays">
                    $forall play <- playWidgets
                        ^{play}
        |]

playbookWidget :: Entity Playbook -> ConnectionPool -> Widget
playbookWidget (Entity playbookid playbook) pool = do
    ((resultRunPlaybook, widgetRunPlaybook), enctype) <- runFormPost $ identifyForm (pack ("runPlaybook" ++ show (fromSqlKey playbookid))) $ buttonForm (fromIntegral (fromSqlKey playbookid))
    case resultRunPlaybook of
        FormSuccess (ButtonForm val) -> do
          runSqlPool (insert $ JobQueue (toSqlKey (fromIntegral val)) "" "") pool
          return ()
        _ -> return ()
    runs <- runSqlPool (selectList [RunPlaybookId ==. playbookid] [Asc RunId]) pool
    toWidget
        [whamlet|
            <li>
                #{playbookPlaybookName playbook}
                <form method=post action=@{HomeR}>
                    ^{widgetRunPlaybook}
                    <button>Run
                <ul class="runs">
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
        _ -> do
            playbooks <- runSqlPool (selectList [PlaybookProjectId ==. projectid] [Asc PlaybookId]) pool
            toWidget
                [whamlet|
                    <li>
                        <ul class="playbooks">
                            Project: #{projectUrl project} (#{projectBranch project})
                            <form method=post action=@{HomeR}>
                                ^{widgetDeleteRepo}
                                <button>Remove
                            $if Prelude.null (projectErrorMessage project)
                                $forall entity <- playbooks
                                    ^{playbookWidget entity pool}
                            $else
                                <font color="red">
                                    #{projectErrorMessage project}
                |]

getHomeR :: Handler Html
getHomeR = do
    ((resultAddRepo, widgetAddRepo), enctype) <- runFormPost $ identifyForm "addRepo" addRepoForm
    case resultAddRepo of
        FormSuccess (AddRepository repo branch) -> runDB ( insert $ Project (unpack repo) (unpack branch) "" "") >> pure ()
        _ -> pure ()
    projects <- runDB $ selectList [] [Asc ProjectId]
    Hansible pool <- getYesod
    defaultLayout
        [whamlet|
            <ul class="projects">
                $forall entity <- projects
                    ^{projectWidget entity pool}
        <form method=post action=@{HomeR} enctype=#{enctype}>
            ^{widgetAddRepo}
            <button>Add
        |]

postHomeR :: Handler Html
postHomeR = getHomeR

runWebserver :: ConnectionPool -> IO ()
runWebserver conn = warp 3000 Hansible { connections = conn }


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
