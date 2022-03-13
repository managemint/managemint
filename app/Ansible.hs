{- app/Ansible.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

{-# LANGUAGE CApiFFI, ForeignFunctionInterface #-}
module Ansible where
import Foreign.C.Types
import Foreign.C.String
import Control.Monad

foreign import ccall "ansible.h ansible" c_ansible :: CString -> CString -> CString -> CString -> IO Int

ansiblePlaybook :: String -> String -> String -> String -> IO Int
ansiblePlaybook _path _pb _limit _tag = do
        join $ c_ansible <$> newCString _path <*> newCString _pb <*> newCString _limit <*> newCString _tag
