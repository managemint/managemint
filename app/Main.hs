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

import Data.IORef
import Data.Text
import Yesod
import Yesod.Core
import Yesod.Form.Jquery
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import Control.Monad.Logger (runStderrLoggingT)

data App = App { connections :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    url String
    branch String
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
    name String
    host String
    runId RunId
    is_changed Bool
    is_skipped Bool
    is_failed Bool
    is_unreachable Bool
    output String
JobQueue
    playbookId PlaybookId
    arguments String
    triggerDate String
|]

data AddRepository = AddRepository
        { repoURL :: Text
        , repoBranch :: Text
        }

addRepoForm = renderDivs $ AddRepository
        <$> areq textField "Repository URL" Nothing
        <*> areq textField "Branch" Nothing

data ButtonForm = ButtonForm
        { hiddenVal :: Int
        }

buttonForm val = renderDivs $ ButtonForm
        <$> areq hiddenField "" (Just val)

getProjects :: ConnectionPool -> IO [Entity Project]
getProjects pool = runSqlPool (selectList [] [Asc ProjectId]) pool

getPlaybooks :: Key Project -> ConnectionPool -> IO [Entity Playbook]
getPlaybooks projectid pool = runSqlPool (selectList [PlaybookProjectId ==. projectid] [Asc PlaybookId]) pool

addRun :: Key Playbook -> Run -> ConnectionPool -> IO (Key Run)
addRun playbookid run pool = runSqlPool (insert $ run) pool

addEvent :: Key Run -> Event -> ConnectionPool -> IO (Key Event)
addEvent runid event pool = runSqlPool (insert $ event) pool

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
    ((resultDeleteRepo, widgetDeleteRepo), enctype) <- runFormPost $ identifyForm (pack ("deleteRepo" ++ (show (fromSqlKey projectid)))) $ buttonForm (fromIntegral (fromSqlKey projectid))
    case resultDeleteRepo of
        FormSuccess (ButtonForm val) -> do
            (runSqlPool (deleteWhere [ProjectId ==. (toSqlKey (fromIntegral val))]) pool)
            [whamlet||]
        _ -> do
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

getHomeR :: Handler Html
getHomeR = do
    ((resultAddRepo, widgetAddRepo), enctype) <- runFormPost $ identifyForm "addRepo" addRepoForm
    case resultAddRepo of
        FormSuccess (AddRepository repo branch) -> (runDB $ insert $ Project (unpack repo) (unpack branch)) >> (pure ())
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
        warp 3000 App { connections = pool }

