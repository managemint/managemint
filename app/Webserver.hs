{- app/Webserver.hs
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
{-# LANGUAGE ViewPatterns      #-}

module Webserver where

import Scheduler
import Config
import Data.Text
import Yesod
import Yesod.Static
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

staticFilePath= "static"

staticFiles "static"

mkYesod "Hansible" [parseRoutes|
/           HomeR GET POST
/run/#Int   RunR GET
/static     StaticR Static getStatic
|]

instance Yesod Hansible

instance RenderMessage Hansible FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist Hansible where
    type YesodPersistBackend Hansible = SqlBackend

    runDB action = do
        Hansible pool _ <- getYesod
        runSqlPool action pool

data Status = Ok | Failed | FailedIgnored | Running

generateStatusIndicator :: Status -> Widget
generateStatusIndicator success =
    let s = case success of
            Ok -> "green"
            Failed -> "red"
            Running -> "blue"
            FailedIgnored -> "#FA5858" :: String in
    toWidget
        [whamlet|
            <font color=#{s}>
                ●
        |]

statusIntToStatus :: Int -> Status
statusIntToStatus x = case x of
                        1 -> Running
                        0 -> Ok
                        -1 -> Failed
                        _ -> Failed

joinStatus :: Status -> Status -> Status
joinStatus Ok Ok = Ok
joinStatus FailedIgnored x = x
joinStatus x FailedIgnored = x
joinStatus _ _ = Failed

statusWidget :: String -> String -> Widget
statusWidget text clazz = do
    toWidget [whamlet|
                <font class=#{clazz}>
                    #{text}
            |]

eventToStatus :: Event -> (Widget, Status)
eventToStatus event
  | eventIs_failed event && eventIgnore_errors event = (statusWidget "FAILED(IGNORED)" "status-failed-ignored", FailedIgnored)
  | eventIs_failed event = (statusWidget "FAILED" "status-failed", Failed)
  | eventIs_changed event = (statusWidget "CHANGED" "status-changed", Ok)
  | eventIs_skipped event = (statusWidget "SKIPPED" "status-skipped", Ok)
  | eventIs_unreachable event = (statusWidget "UNREACHABLE" "status-unreachable", Failed)
  | otherwise = (statusWidget "OK" "status-ok", Ok)

itemWidget :: Entity Event -> IO (Widget, Status)
itemWidget (Entity eventid event) = do
    let (text, status) = eventToStatus event
    return (toWidget [whamlet|#{eventItem event}: ^{text}|], status)

hostWidget :: Entity Run -> Int -> Int -> String -> ConnectionPool -> IO (Widget, Status)
hostWidget (Entity runid run) playId taskId host pool = do
    isItemized <- runSqlPool (exists [EventPlay_id ==. playId, EventTask_id ==. taskId, EventHost ==. host, EventRunId ==. runid, EventIs_item ==. True]) pool
    -- Either single element or sublist in case of items
    if isItemized then
        do
            events <- runSqlPool (selectList [EventPlay_id ==. playId, EventTask_id ==. taskId, EventHost ==. host, EventRunId ==. runid, EventIs_item ==. True] [Asc EventId]) pool
            items <- mapM itemWidget events
            let (hostWidgets, status) = Data.Foldable.foldr (\(w, s) (ws, ss) -> (w:ws, joinStatus s ss)) ([], Ok) items
            return (toWidget [whamlet|
                <ul>
                    $forall x <- hostWidgets
                        <li>
                            ^{x}
            |], status)
    else
        do
            event <- runSqlPool (selectFirst [EventPlay_id ==. playId, EventTask_id ==. taskId, EventHost ==. host, EventRunId ==. runid] [Asc EventId]) pool
            let (text, status) = eventToStatus (entityVal (fromJust event))
            return (toWidget [whamlet|#{eventHost (entityVal (fromJust event))}: ^{text}|], status)

getHosts :: MonadIO m => Key Run -> Int -> Int -> ReaderT SqlBackend m [Single String]
getHosts run playId taskId = rawSql "SELECT DISTINCT event.host FROM event WHERE event.run_id=? AND event.play_id=? AND event.task_id=?" [toPersistValue run, toPersistValue playId,toPersistValue taskId]

taskWidget :: Entity Run -> Int -> Int -> ConnectionPool -> IO (Widget, Status)
taskWidget entity@(Entity runid run) playId taskId pool = do
    event <- runSqlPool (selectFirst [EventPlay_id ==. playId, EventTask_id ==. taskId, EventRunId ==. runid] []) pool
    hostNames <- runSqlPool (getHosts runid playId taskId) pool
    hosts <- mapM (\x -> hostWidget entity playId taskId (unSingle x) pool) hostNames
    let (hostWidgets, status) = Data.Foldable.foldr (\(w, s) (ws, ss) -> (w:ws, joinStatus s ss)) ([], Ok) hosts
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
    let (taskWidgets, status) = Data.Foldable.foldr (\(w, s) (ws, ss) -> (w:ws, joinStatus s ss)) ([], Ok) tasks
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
    let (playWidgets, _treeStatus) = Data.Foldable.foldr (\(w, s) (ws, ss) -> (w:ws, joinStatus s ss)) ([], Ok) plays
    toWidget
        [whamlet|
            <li>
                #{runTriggerdate run} ^{generateStatusIndicator (statusIntToStatus (runStatus run))}
                <ul class="plays">
                    $forall play <- playWidgets
                        ^{play}
        |]

playbookWidget :: Entity Playbook -> ConnectionPool -> Widget
playbookWidget (Entity playbookid playbook) pool = do
    ((_, widgetRunPlaybook), enctype) <- runFormPost $ identifyForm (pack "runPlaybook") $ buttonForm (fromIntegral (fromSqlKey playbookid))
    runs <- runSqlPool (selectList [RunPlaybookId ==. playbookid] [Desc RunId]) pool
    toWidget
        [whamlet|
            <li>
                #{playbookPlaybookName playbook}
                <form method=post action=@{HomeR}>
                    ^{widgetRunPlaybook}
                    <button>Run
                <ul class="runs">
                    $forall (Entity entityid entity) <- runs
                        <li>
                            <a href=@{RunR (fromIntegral (fromSqlKey entityid))}>
                                #{runTriggerdate entity}
                            ^{generateStatusIndicator (statusIntToStatus (runStatus entity))}
        |]

projectWidget :: Entity Project -> ConnectionPool -> Widget
projectWidget (Entity projectid project) pool = do
    ((_, widgetDeleteRepo), enctype) <- runFormPost $ identifyForm (pack "deleteRepo") $ buttonForm (fromIntegral (fromSqlKey projectid))
    playbooks <- runSqlPool (selectList [PlaybookProjectId ==. projectid] [Asc PlaybookId]) pool
    toWidget
        [whamlet|
            <li>
                Project: #{projectUrl project} (#{projectBranch project})
                <form method=post action=@{HomeR}>
                    ^{widgetDeleteRepo}
                    <button>Remove
                $if Prelude.null (projectErrorMessage project)
                    <ul class="playbooks">
                        $forall entity <- playbooks
                            ^{playbookWidget entity pool}
                $else
                    <br>
                    <font color="red">
                        #{projectErrorMessage project}
        |]

hansibleStyle :: Widget -> Widget
hansibleStyle inp = do
    addStylesheet $ StaticR style_css
    inp

headerWidget :: Widget
headerWidget = toWidget
    [whamlet|
        <div class="header">
            <p class="header">
                <a href=/>
                    <img class="header" src=@{StaticR logo_png}/></a>hansible
    |]

-- Every node in the tree is generated by a widget*function which generates a widget with a status and calls the widget* function for all children
getHomeR :: Handler Html
getHomeR = do
    ((_, widgetAddRepo), enctype) <- runFormPost $ identifyForm "addRepo" addRepoForm
    projects <- runDB $ selectList [] [Asc ProjectId]
    Hansible pool _ <- getYesod
    defaultLayout
      (hansibleStyle
        [whamlet|
            ^{headerWidget}
            <div class="content">
                <ul class="projects">
                    $forall entity <- projects
                        ^{projectWidget entity pool}
                <br>
                <form method=post action=@{HomeR} enctype=#{enctype} id="addrepo">
                    ^{widgetAddRepo}
                    <button>Add
        |])

-- We handle the forms here in POST and then redirect over to GET. This allows refreshing without resending data
postHomeR :: Handler Html
postHomeR = do
    ((resultAddRepo, _), _) <- runFormPost $ identifyForm "addRepo" addRepoForm
    case resultAddRepo of
        FormSuccess (AddRepository repo branch) -> do
            runDB ( insert $ Project (unpack repo) (unpack branch) "" "")
            return ()
        _ -> return ()
    ((resultDeleteRepo, _), _) <- runFormPost $ identifyForm (pack "deleteRepo") $ buttonForm 0
    case resultDeleteRepo of
        FormSuccess (ButtonForm val) -> do
            runDB (deleteWhere [ProjectId ==. toSqlKey (fromIntegral val)])
        _ -> return ()
    ((resultRunPlaybook, _), enctype) <- runFormPost $ identifyForm (pack "runPlaybook") $ buttonForm 0
    case resultRunPlaybook of
        FormSuccess (ButtonForm val) -> do
            runDB (insert $ JobQueue (toSqlKey (fromIntegral val)) "" "")
            return ()
        _ -> return ()
    redirect HomeR

getRunR :: Int -> Handler Html
getRunR runInt= do
    let runId = toSqlKey $ fromIntegral runInt
    row <- runDB $ selectFirst [RunId  ==. runId] []
    Hansible pool _ <- getYesod
    defaultLayout $ hansibleStyle
        [whamlet|
            ^{headerWidget}
            <div class="content">
                $case row
                    $of Just entity
                        ^{runWidget entity pool}
                    $of _
                        ^{headerWidget}
                        Can't find run
        |]

runWebserver :: ConnectionPool -> IO ()
runWebserver conn = do
    x <- static staticFilePath
    warp webserverPort Hansible { connections = conn, getStatic = x }

