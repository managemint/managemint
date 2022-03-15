{- app/Git.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE CApiFFI, ForeignFunctionInterface #-}
module Git(isRepo, doClone, doPull, getLastOid, GitException) where
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.MySQL (SqlBackend)

type GitException = ExceptT String (ReaderT SqlBackend IO)

-- TODO Free CStrings?

-- path -> url -> result
foreign import ccall "git.h is_repo" c_is_repo :: CString -> CString -> IO Int

-- url -> path -> refspec -> result
foreign import ccall "git.h do_git_clone" c_do_git_clone :: CString -> CString -> CString -> IO Int

-- path -> refspec
foreign import ccall "git.h do_git_pull" c_do_git_pull :: CString -> CString -> IO Int

foreign import ccall "git.h get_last_merge_oid" c_get_last_merge_oid :: IO CString

-- Arguments: Target path, Repo URL
isRepo :: String -> String -> IO Bool
isRepo _path _url = do
    arg@[path, url] <- mapM newCAString [_path, _url]
    ret <- c_is_repo path url
    mapM_ free arg
    return $ (==0) ret

-- Args: Repo URL, target path, refspec (branch)
doClone :: String -> String -> String -> GitException ()
doClone _url _path _refspec = do
    arg@[path, url, refspec] <- liftIO $mapM newCAString [_path, _url, _refspec]
    ret <- liftIO $ c_do_git_clone url path refspec
    liftIO $ mapM_ free arg
    when (ret /= 0) $ throwError "Clone failed"

-- Args: local repo path, refspec (branch)
doPull :: String -> String -> GitException ()
doPull _path _refspec = do
    arg@[path, refspec] <- liftIO $ mapM newCAString [_path, _refspec]
    ret <- liftIO $ c_do_git_pull path refspec
    liftIO $ mapM_ free arg
    when (ret /= 0) $ throwError "Pill failed"

getLastOid :: IO String
getLastOid = do
    cs <- c_get_last_merge_oid
    s <- peekCAString cs
    free cs
    return s
