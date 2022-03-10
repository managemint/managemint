{-# LANGUAGE CApiFFI, ForeignFunctionInterface #-}
module Git where
import Foreign.C.Types
import Foreign.C.String
import Control.Monad

-- TODO Free CStrings?

-- path -> url -> result
foreign import capi "git.h is_repo" c_is_repo :: CString -> CString -> IO Int

-- url -> path -> refspec -> result
foreign import capi "git.h do_git_clone" c_do_git_clone :: CString -> CString -> CString -> IO Int

-- path -> refspec
foreign import capi "git.h do_git_pull" c_do_git_pull :: CString -> CString -> IO Int

-- Return: 0: OK -1: Not a repo -2: does not have specified remote
isRepo :: String -> String -> IO Int
isRepo _path _url = join $ c_is_repo <$> newCString _path <*> newCString _url

doClone :: String -> String -> String -> IO Int
doClone _url _path _refspec = join $ c_do_git_clone <$> newCString _url <*> newCString _path <*> newCString _refspec

doPull :: String -> String -> IO Int
doPull _path _refspec = join $ c_is_repo <$> newCString _path <*> newCString _refspec
