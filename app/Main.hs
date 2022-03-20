{-# LANGUAGE LambdaCase #-}
{- app/Main.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

import Scheduler
import Yesod
import DatabaseUtil
import Database.Persist
import Database.Persist.MySQL
import Control.Monad.Logger (runStderrLoggingT, runNoLoggingT)
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, poll, Async)
import Webserver

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

schedu = threadDelay 20000000
websev = threadDelay 10000000

main :: IO ()
main = do
    runStderrLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
        runSqlPersistMPool (runMigration migrateAll) pool
        sched <- async $ schedule pool
        websv <- async $ runWebserver pool

        monitorStatus [sched, websv]
