{- app/loggerUtil.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE KindSignatures, RankNTypes #-}

module LoggerUtil( runHansibleLogger
                 , getCurrentLogger
                 , rerunHansibleLogger
                 , asyncWithLogger
                 , hLogDebug, hLogInfo, hLogWarn, hLogError
                 , LoggingT
                 , MonadLogger
                 ) where

import Config
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (
    LoggingT, runStderrLoggingT, runNoLoggingT, runLoggingT,
    filterLogger, LogSource, LogLevel, askLoggerIO, MonadLogger,
    MonadLoggerIO, Loc, LogStr, logWarnNS, logErrorNS, logInfoNS, logDebugNS)
import Control.Concurrent.Async (async, Async)

import Data.Text (Text(..), pack)


logFilterSource :: Text -> Bool
logFilterSource = flip notElem loggerUtilLogSourcesBlocklist

logFilterLevel :: LogLevel -> Bool
logFilterLevel = (<=) loggerUtilLogLevel

logFilter :: LogSource -> LogLevel -> Bool
logFilter s l = logFilterLevel l && logFilterSource s

-- | Rerun with logger from getCurrentLogger
-- User like so:
-- @
-- lg <- getCurrentLogger
-- as <- liftIO $ async $ rerunHansibleLogger (f) lg
-- @
rerunHansibleLogger :: forall (m :: * -> *) a. LoggingT m a -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> m a
rerunHansibleLogger = runLoggingT

runHansibleLogger :: forall (m :: * -> *) a. MonadIO m => LoggingT m a -> m a
runHansibleLogger f = runStderrLoggingT (filterLogger logFilter f)

getCurrentLogger :: MonadLoggerIO m => m (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
getCurrentLogger = askLoggerIO

-- | Wrapper to allow async calls to pass Logger.
-- equivalent to
-- @
-- lg <- askLoggerIO
-- as  <- liftIO $ async $ runLoggingT (f) lg
-- @
asyncWithLogger :: forall a. LoggingT IO a -> LoggingT IO (Async a)
asyncWithLogger f = do
        logger <- getCurrentLogger
        liftIO $ async $ rerunHansibleLogger f logger


hLogDebug :: MonadLogger m => String -> String -> m ()
hLogDebug s m = logDebugNS (pack s) (pack m)

hLogInfo :: MonadLogger m => String -> String -> m ()
hLogInfo s m = logInfoNS (pack s) (pack m)

hLogWarn :: MonadLogger m => String -> String -> m ()
hLogWarn s m = logWarnNS (pack s) (pack m)

hLogError :: MonadLogger m => String -> String -> m ()
hLogError s m = logErrorNS (pack s) (pack m)
