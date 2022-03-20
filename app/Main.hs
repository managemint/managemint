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
import Control.Monad.Logger (runStderrLoggingT)
import Control.Concurrent.Async
import Webserver

main :: IO ()
main = do
    runStderrLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
        runSqlPersistMPool (runMigration migrateAll) pool
        _ <- async $ schedule pool
        runWebserver pool
