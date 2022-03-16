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

import Scheduler
import Data.Text
import Yesod
import DatabaseUtil
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import Control.Monad.Logger (runStderrLoggingT)
import Control.Concurrent.Async

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
                                Project: #{projectUrl project} (#{projectBranch project})
                                <form method=post action=@{HomeR}>
                                    ^{widgetDeleteRepo}
                                    <button>Remove
                                $forall entity <- playbooks
                                    TestPlaybook
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
