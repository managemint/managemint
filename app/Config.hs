{- app/Config.hs
 -
 - Copyright (C) 2022 Jonas Gunz, Konstantin Grabmann, Paul Trojahn
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License version 3 as
 - published by the Free Software Foundation.
 -
 -}

module Config where
-- Format: <module><Name>

sockReadLen :: Int
sockReadLen = 2048
schedulerFailMax = 3 :: Int
schedulerUserTemplateKey = "USERTEMPLATE" -- Parser is not allowed to parse a job name starting with this string

executorSockPath = "/tmp/hansible.sock"
