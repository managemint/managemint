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
module Git(isRepo, doClone, doPull, getLastOid) where
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc

-- path -> url -> result
foreign import ccall "git.h is_repo" c_is_repo :: CString -> CString -> IO Int

-- url -> path -> refspec -> result
foreign import ccall "git.h do_git_clone" c_do_git_clone :: CString -> CString -> CString -> IO Int

-- path -> refspec
foreign import ccall "git.h do_git_pull" c_do_git_pull :: CString -> CString -> IO Int

foreign import ccall "git.h get_last_merge_oid" c_get_last_merge_oid :: IO CString

-- Call only, when return is c_hsgit_call_failed
foreign import ccall "git.h get_last_error" c_get_last_error :: IO CString

-- import error defines
foreign import capi "git.h value HSGIT_OK" c_hsgit_ok :: Int
foreign import capi "git.h value HSGIT_CALL_FAILED" c_hsgit_call_failed :: Int
foreign import capi "git.h value HSGIT_INCORRECT_REMOTE" c_hsgit_incorrect_remote :: Int
foreign import capi "git.h value HSGIT_REFSPEC_NOT_MERGEABLE" c_hsgit_refspec_not_mergeable :: Int
foreign import capi "git.h value HSGIT_NO_FF_POSSIBLE" c_hsgit_no_ff_possible :: Int

-- | Internal Errors defined in C Source
internalErrstr :: Int -> String
internalErrstr i
    | i == c_hsgit_refspec_not_mergeable = "Refspec is not mergable"
    | i == c_hsgit_incorrect_remote = "Specified remote is not set"
    | i == c_hsgit_call_failed = "libgit error"
    | i == c_hsgit_no_ff_possible = "No Fast-Forward possible"
    | i == c_hsgit_ok = "OK"
    | otherwise = "Unknown Error: " ++ show  i

-- | Get libgit2 errstring
getLastErrstr :: IO String
getLastErrstr = peekCAString =<< c_get_last_error

-- | Determines, which errstring resolver to use
handleError :: Int -> IO (Either String ())
handleError i
    | i == c_hsgit_ok = return $ Right ()
    | i == c_hsgit_call_failed = fmap Left getLastErrstr
    | otherwise = return $ Left $ internalErrstr i

-- | Check, wether repo at _path is a repo and has _url as remote
-- | Arguments: Target path -> Repo URL
-- | Return: True, if OK, False otherwise
isRepo :: String -> String -> IO Bool
isRepo _path _url = do
    arg@[path, url] <- mapM newCAString [_path, _url]
    ret <- c_is_repo path url
    mapM_ free arg
    return $ (==0) ret


-- | Perform Git-Clone of _url into folder _path, checkout branch _refspec
-- | Args: Repo URL, target path, refspec (branch)
-- | Return: () if OK, Errstring if failed
doClone :: String -> String -> String -> IO (Either String ())
doClone _url _path _refspec = do
    arg@[path, url, refspec] <- mapM newCAString [_path, _url, _refspec]
    ret <- c_do_git_clone url path refspec
    mapM_ free arg
    handleError ret

-- | Perform git pull on repo in _path on branch _refspec
-- | Args: local repo path, refspec (branch)
-- | Return: () if OK, Errstring if failed
doPull :: String -> String -> IO (Either String ())
doPull _path _refspec = do
    arg@[path, refspec] <- mapM newCAString [_path, _refspec]
    ret <- c_do_git_pull path refspec
    mapM_ free arg
    handleError ret

-- | get Git Object-ID (sha256 hash) of last pull's HEAD
-- | Only valid after doPull succeeded.
-- | Return: Hex OID as String
getLastOid :: IO String
getLastOid = do
    cs <- c_get_last_merge_oid
    s <- peekCAString cs
    free cs
    return s
