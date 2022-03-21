{- app/Main.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

import Database.Persist.MySQL (withMySQLPool, runSqlPersistMPool, runMigration)

import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT, filterLogger, LogSource, LogLevel)
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, poll, Async)
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)

import Webserver
import Scheduler
import Config
import DatabaseUtil


logFilterSource :: Text -> Bool
logFilterSource = flip notElem mainLogSourcesBlocklist

logFilterLevel :: LogLevel -> Bool
logFilterLevel = (<=) mainLogLevel

logFilter :: LogSource -> LogLevel -> Bool
logFilter s l = logFilterLevel l && logFilterSource s


checkThreadOk :: (Show a, Show b) => Maybe (Either a b) -> IO Bool
checkThreadOk (Just (Left e)) = do
    print $ "Thread Exited with Exception " ++ show e
    return False
checkThreadOk (Just (Right r)) = do
    print $ "Thread Exited normally with value " ++ show r
    return False
checkThreadOk Nothing = return True

-- | Check status of list of Asyncs, exit when one fails
-- to not drag along a half-crashed application
monitorStatus :: (Show a) => [Async a] -> IO ()
monitorStatus as = do
    threadDelay 1000000
    (mapM checkThreadOk =<< mapM poll as) >>= (`when` monitorStatus as) . and


main :: IO ()
main = do
    runStderrLoggingT $ filterLogger logFilter $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
        runSqlPersistMPool (runMigration migrateAll) pool
        sched <- async $ schedule pool
        websv <- async $ runWebserver pool

        monitorStatus [sched, websv]
