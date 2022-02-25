{-# LANGUAGE CApiFFI, ForeignFunctionInterface #-}
module Ansible where
import Foreign.C.Types
import Foreign.C.String
import Control.Monad

foreign import capi "ansible.h ansible" c_ansible :: CString -> CString -> CString -> CString -> IO Int

ansiblePlaybook :: String -> String -> String -> String -> IO Int
ansiblePlaybook _path _pb _limit _tag = do
        join $ c_ansible <$> newCString _path <*> newCString _pb <*> newCString _limit <*> newCString _tag
